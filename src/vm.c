#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "memory.h"
#include "object.h"
#include "stdlib.h"
#include "vm.h"

VM vm;

static void resetStack() {
    vm.stackTop = vm.stack;
    vm.frameCount = 0;
    vm.openUpvalues = NULL;
}

void runtimeError(const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    for (int i = vm.frameCount - 1; i >= 0; i--) {
        CallFrame* frame = &vm.frames[i];
        ObjFunction* function = frame->closure->function;
        size_t instruction = frame->ip - function->chunk.code - 1;
        fprintf(stderr, "[line %d] in ",
            getLine(&function->chunk, instruction));
        if (function->name == NULL) {
            fprintf(stderr, "script\n");
        } else {
            fprintf(stderr, "%s()\n", function->name->chars);
        }
    }

    resetStack();
}

static bool hasError() {
    return vm.frameCount == 0;
}

void defineNative(const char* name, NativeFn function, int arity) {
    push(OBJ_VAL(copyString(name, (int)strlen(name))));
    push(OBJ_VAL(newNative(function, arity)));
    tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);
    pop();
    pop();
}

void initVM() {
    resetStack();
    vm.objects = NULL;
    vm.bytesAllocated = 0;
    vm.nextGC = 1024 * 1024;

    vm.grayCount = 0;
    vm.grayCapacity = 0;
    vm.grayStack = NULL;

    initTable(&vm.globals);
    initTable(&vm.strings);

    vm.initString = NULL; // copyString can cause garbage collection.
    vm.initString = copyString("init", 4);

    initStdlib();
}

void freeVM() {
    freeTable(&vm.globals);
    freeTable(&vm.strings);
    FREE_ARRAY(Value, vm.stack, vm.stackEnd - vm.stack);
    freeObjects();
}

void push(Value value) {
    if (vm.stackEnd <= vm.stackTop) {
        Value* oldStack = vm.stack;

        int stackSize = vm.stackTop - vm.stack;
        int oldCapacity = vm.stackEnd - vm.stack;

        int newCapacity = GROW_CAPACITY(oldCapacity);
        vm.stack = (Value*)realloc(vm.stack, sizeof(Value) * newCapacity);
        if (vm.stack == NULL) {
            fprintf(stderr, "Out of memory.");
            exit(1);
        }

        vm.stackTop = vm.stack + stackSize;
        vm.stackEnd = vm.stack + newCapacity;

        size_t offset = vm.stack - oldStack;

        ObjUpvalue* upvalue = vm.openUpvalues;
        while (upvalue != NULL) {
            upvalue->location += offset;
            upvalue = upvalue->next;
        }

        for (int i = 0; i < vm.frameCount; i++) {
            vm.frames[i].slots += offset;
        }
    }

    *vm.stackTop = value;
    vm.stackTop++;
}

Value pop() {
    vm.stackTop--;
    return *vm.stackTop;
}

static Value peek(int distance) {
    return vm.stackTop[-1 - distance];
}

static void replace(int distance, Value value) {
    vm.stackTop[-1 - distance] = value;
}

static bool call(ObjClosure* closure, int argCount) {
    if (argCount != closure->function->arity) {
        runtimeError("Expected %d arguments, but got %d.",
            closure->function->arity, argCount);
        return false;
    }

    if (vm.frameCount == FRAMES_MAX) {
        runtimeError("Stack overflow.");
        return false;
    }

    CallFrame* frame = &vm.frames[vm.frameCount++];
    frame->closure = closure;
    frame->ip = closure->function->chunk.code;
    frame->slots = vm.stackTop - argCount - 1;
    return true;
}

static bool callNative(ObjNative* native, int argCount) {
    if (argCount != native->arity) {
        runtimeError("Expected %d arguments, but got %d.",
            native->arity, argCount);
        return false;
    }

    Value result = native->function(vm.stackTop - argCount);
    if (hasError()) return false;

    vm.stackTop -= argCount + 1;
    push(result);
    return true;
}

/**
 * Call a value making the top argCount + 1 slots on the stack the bottom of
 * the callee's stack, then remove all that from the caller's stack and push the
 * return value.
 *
 * Return true on success, false on error.
 */
static bool callValue(Value callee, int argCount) {
    if (IS_OBJ(callee)) {
        switch (OBJ_TYPE(callee)) {
            case OBJ_BOUND_METHOD: {
                ObjBoundMethod* bound = AS_BOUND_METHOD(callee);
                replace(argCount, bound->receiver);
                return call(bound->method, argCount);
            }
            case OBJ_CLASS: {
                ObjClass* klass = AS_CLASS(callee);
                replace(argCount, OBJ_VAL(newInstance(klass)));
                Value initializer;
                if (tableGet(&klass->methods, vm.initString,
                             &initializer)) {
                    return call(AS_CLOSURE(initializer), argCount);
                }

                if (argCount != 0) {
                    runtimeError("Expected 0 arguments, but got %d.",
                                 argCount);
                    return false;
                }

                return true;
            }
            case OBJ_CLOSURE:
                return call(AS_CLOSURE(callee), argCount);
            case OBJ_NATIVE:
                return callNative(AS_NATIVE(callee), argCount);
            default:
                break; // Non-callable object type.
        }
    }
    runtimeError("Can only call functions and classes.");
    return false;
}

/**
 * Call the method with the given name from the given class on peek(argCount)
 * with argCount arguments.
 */
static bool invokeFromClass(ObjClass* klass, ObjString* name,
                            int argCount) {
    Value method;
    if (!tableGet(&klass->methods, name, &method)) {
        runtimeError("Undefined property '%s'.", name->chars);
        return false;
    }
    return call(AS_CLOSURE(method), argCount);
}

/**
 * Call the property with the given name of peek(argCount) with argCount
 * arguments. Error if peek(argCount) is not a class instance or the
 * property was not found. For methods bind them to peek(argCount) first.
 *
 * Return true on success, false on error.
 */
static bool invoke(ObjString* name, int argCount) {
    Value receiver = peek(argCount);

    if (!IS_INSTANCE(receiver)) {
        runtimeError("Only instances have methods.");
        return false;
    }

    ObjInstance* instance = AS_INSTANCE(receiver);

    Value value;
    if (tableGet(&instance->fields, name, &value)) {
        replace(argCount, value);
        return callValue(value, argCount);
    }

    return invokeFromClass(instance->klass, name, argCount);
}

/**
 * Bind the method with the given name of the given class to the value on the
 * top of the stack and replace the value with it.
 *
 * Return true on success, false if the method was not found.
 */
static bool bindMethod(ObjClass* klass, ObjString* name) {
    Value method;
    if (!tableGet(&klass->methods, name, &method)) {
        runtimeError("Undefined property '%s'.", name->chars);
        return false;
    }

    ObjBoundMethod* bound = newBoundMethod(peek(0),
                                           AS_CLOSURE(method));
    replace(0, OBJ_VAL(bound));
    return true;
}

static ObjUpvalue* captureUpvalue(Value* local) {
    ObjUpvalue* prevUpvalue = NULL;
    ObjUpvalue* upvalue = vm.openUpvalues;
    while (upvalue != NULL && upvalue->location > local) {
        prevUpvalue = upvalue;
        upvalue = upvalue->next;
    }

    if (upvalue != NULL && upvalue->location == local) {
        return upvalue;
    }

    ObjUpvalue* createdUpvalue = newUpvalue(local);
    createdUpvalue->next = upvalue;

    if (prevUpvalue == NULL) {
        vm.openUpvalues = createdUpvalue;
    } else {
        prevUpvalue->next = createdUpvalue;
    }

    return createdUpvalue;
}

static void closeUpvalues(Value* last) {
    while (vm.openUpvalues != NULL &&
           vm.openUpvalues->location >= last) {
        ObjUpvalue* upvalue = vm.openUpvalues;
        upvalue->closed = *upvalue->location;
        upvalue->location = &upvalue->closed;
        vm.openUpvalues = upvalue->next;
    }
}

static void defineMethod(ObjString* name) {
    Value method = peek(0);
    ObjClass* klass = AS_CLASS(peek(1));
    tableSet(&klass->methods, name, method);
    pop();
}

static bool isFalsey(Value value) {
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void concatenate() {
    ObjString* b = AS_STRING(peek(0));
    ObjString* a = AS_STRING(peek(1));

    int length = a->length + b->length;
    char* chars = ALLOCATE(char, length + 1);
    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';

    ObjString* result = takeString(chars, length);
    vm.stackTop--;
    replace(0, OBJ_VAL(result));
}

static InterpretResult run() {
    CallFrame* frame = &vm.frames[vm.frameCount - 1];
    register uint8_t* ip = frame->ip;

#define RUNTIME_ERROR(...) \
    do { \
        frame->ip = ip; \
        runtimeError(__VA_ARGS__); \
        return INTERPRET_RUNTIME_ERROR; \
    } while (true);

/**
 * Do `call` assuming that it can fail.
 */
#define TRY(call) \
    do { \
        frame->ip = ip; \
        if (!(call)) return INTERPRET_RUNTIME_ERROR; \
    } while (false)

/**
 * Do `call` assuming that it can fail and affects the call stack.
 */
#define LOX_CALL(call) \
    do { \
        TRY(call); \
        frame = &vm.frames[vm.frameCount - 1]; \
        ip = frame->ip; \
    } while (false)

#define READ_BYTE() (*ip++)
#define READ_SHORT() \
    (ip += 2, (uint16_t)((ip[-1] << 8) | ip[-2]))

#define READ_CONSTANT() \
    (frame->closure->function->chunk.constants.values[READ_BYTE()])

#define READ_STRING() AS_STRING(READ_CONSTANT())
#define BINARY_OP(valueType, op) \
    do { \
        Value valB = peek(0); \
        Value valA = peek(1); \
        if (!IS_NUMBER(valB) || !IS_NUMBER(valA)) { \
            RUNTIME_ERROR("Operands must be numbers."); \
        } \
        double b = AS_NUMBER(valB); \
        double a = AS_NUMBER(valA); \
        vm.stackTop--; \
        replace(0, valueType(a op b)); \
    } while (false)

    for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
        printf("          ");
        for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
            printf("[ ");
            printValue(*slot);
            printf(" ]");
        }
        printf("\n");
        disassembleInstruction(&frame->closure->function->chunk,
            (int)(ip - frame->closure->function->chunk.code));
#endif

        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
            case OP_CONSTANT: {
                Value constant = READ_CONSTANT();
                push(constant);
                break;
            }
            case OP_NIL:   push(NIL_VAL); break;
            case OP_TRUE:  push(BOOL_VAL(true)); break;
            case OP_FALSE: push(BOOL_VAL(false)); break;
            case OP_POP: pop(); break;
            case OP_DUPLICATE: push(peek(0)); break;
            case OP_GET_LOCAL: {
                uint8_t slot = READ_BYTE();
                push(frame->slots[slot]);
                break;
            }
            case OP_SET_LOCAL: {
                uint8_t slot = READ_BYTE();
                frame->slots[slot] = peek(0);
                break;
            }
            case OP_GET_GLOBAL: {
                ObjString* name = READ_STRING();
                Value value;
                if (!tableGet(&vm.globals, name, &value)) {
                    RUNTIME_ERROR("Undefined variable '%s'.", name->chars);
                }
                push(value);
                break;
            }
            case OP_DEFINE_GLOBAL: {
                ObjString* name = READ_STRING();
                tableSet(&vm.globals, name, peek(0));
                pop();
                break;
            }
            case OP_SET_GLOBAL: {
                ObjString* name = READ_STRING();
                if (tableSet(&vm.globals, name, peek(0))) {
                    tableDelete(&vm.globals, name);
                    RUNTIME_ERROR("Undefined variable '%s'.", name->chars);
                }
                break;
            }
            case OP_GET_UPVALUE: {
                uint8_t slot = READ_BYTE();
                push(*frame->closure->upvalues[slot]->location);
                break;
            }
            case OP_SET_UPVALUE: {
                uint8_t slot = READ_BYTE();
                *frame->closure->upvalues[slot]->location = peek(0);
                break;
            }
            case OP_GET_PROPERTY: {
                if (!IS_INSTANCE(peek(0))) {
                    RUNTIME_ERROR("Only instances have properties.");
                }

                ObjInstance* instance = AS_INSTANCE(peek(0));
                ObjString* name = READ_STRING();

                Value value;
                if (tableGet(&instance->fields, name, &value)) {
                    replace(0, value);
                    break;
                }

                TRY(bindMethod(instance->klass, name));
                break;
            }
            case OP_SET_PROPERTY: {
                if (!IS_INSTANCE(peek(1))) {
                    RUNTIME_ERROR("Only instances have fields.");
                }

                ObjInstance* instance = AS_INSTANCE(peek(1));
                tableSet(&instance->fields, READ_STRING(), peek(0));
                Value value = pop();
                replace(0, value);
                break;
            }
            case OP_GET_SUPER: {
                ObjString* name = READ_STRING();
                ObjClass* superclass = AS_CLASS(pop());

                TRY(bindMethod(superclass, name));
                break;
            }
            case OP_EQUAL: {
                Value b = peek(0);
                Value a = peek(1);
                vm.stackTop--;
                replace(0, BOOL_VAL(valuesEqual(a, b)));
                break;
            }
            case OP_GREATER:  BINARY_OP(BOOL_VAL, >); break;
            case OP_LESS:     BINARY_OP(BOOL_VAL, <); break;
            case OP_ADD: {
                Value b = peek(0);
                Value a = peek(1);
                if (IS_STRING(a) && IS_STRING(b)) {
                    concatenate();
                } else if (IS_NUMBER(a) && IS_NUMBER(b)) {
                    double aNumber = AS_NUMBER(a);
                    double bNumber = AS_NUMBER(b);
                    vm.stackTop--;
                    replace(0, NUMBER_VAL(aNumber + bNumber));
                } else {
                    RUNTIME_ERROR(
                        "Operands must be two numbers or two strings.");
                }

                break;
            }
            case OP_SUBTRACT: BINARY_OP(NUMBER_VAL, -); break;
            case OP_MULTIPLY: BINARY_OP(NUMBER_VAL, *); break;
            case OP_DIVIDE:   BINARY_OP(NUMBER_VAL, /); break;
            case OP_NOT:
                replace(0, BOOL_VAL(isFalsey(peek(0))));
                break;
            case OP_NEGATE: {
                Value operand = peek(0);
                if (!IS_NUMBER(operand)) {
                    RUNTIME_ERROR("Operand must be a number.");
                }
                replace(0, NUMBER_VAL(-AS_NUMBER(operand)));
                break;
            }
            case OP_PRINT: {
                printValue(pop());
                printf("\n");
                break;
            }
            case OP_JUMP: {
                uint16_t offset = READ_SHORT();
                ip += offset;
                break;
            }
            case OP_JUMP_IF_FALSE: {
                uint16_t offset = READ_SHORT();
                if (isFalsey(peek(0))) ip += offset;
                break;
            }
            case OP_LOOP: {
                uint16_t offset = READ_SHORT();
                ip -= offset;
                break;
            }
            case OP_CALL: {
                int argCount = READ_BYTE();
                LOX_CALL(callValue(peek(argCount), argCount));
                break;
            }
            case OP_INVOKE: {
                ObjString* method = READ_STRING();
                int argCount = READ_BYTE();
                LOX_CALL(invoke(method, argCount));
                break;
            }
            case OP_SUPER_INVOKE: {
                ObjString* method = READ_STRING();
                int argCount = READ_BYTE();
                ObjClass* superclass = AS_CLASS(pop());
                LOX_CALL(invokeFromClass(superclass, method, argCount));
                break;
            }
            case OP_CLOSURE: {
                ObjFunction* function = AS_FUNCTION(READ_CONSTANT());
                ObjClosure* closure = newClosure(function);
                push(OBJ_VAL(closure));
                for (int i = 0; i < closure->upvalueCount; i++) {
                    uint8_t isLocal = READ_BYTE();
                    uint8_t index = READ_BYTE();
                    if (isLocal) {
                        closure->upvalues[i] =
                            captureUpvalue(frame->slots + index);
                    } else {
                        closure->upvalues[i] = frame->closure->upvalues[index];
                    }
                }
                break;
            }
            case OP_CLOSE_UPVALUE:
                closeUpvalues(vm.stackTop - 1);
                pop();
                break;
            case OP_RETURN: {
                Value result = pop();
                closeUpvalues(frame->slots);
                vm.frameCount--;
                if (vm.frameCount == 0) {
                    pop();
                    return INTERPRET_OK;
                }

                vm.stackTop = frame->slots;
                push(result);
                frame = &vm.frames[vm.frameCount - 1];
                ip = frame->ip;
                break;
            }
            case OP_CLASS:
                push(OBJ_VAL(newClass(READ_STRING())));
                break;
            case OP_INHERIT: {
                Value superclass = peek(1);
                if (!IS_CLASS(superclass)) {
                    RUNTIME_ERROR("Superclass must be a class.");
                }

                ObjClass* subclass = AS_CLASS(peek(0));
                tableAddAll(&AS_CLASS(superclass)->methods,
                            &subclass->methods);
                pop(); // Subclass.
                break;
            case OP_METHOD:
                defineMethod(READ_STRING());
                break;
            }
        }
    }

#undef RUNTIME_ERROR
#undef TRY
#undef LOX_CALL
#undef READ_BYTE
#undef READ_SHORT
#undef READ_LONG
#undef READ_CONSTANT
#undef READ_CONSTANT_LONG
#undef READ_STRING
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
    ObjFunction* function = compile(source);
    if (!function) return INTERPRET_COMPILE_ERROR;

    push(OBJ_VAL(function));
    ObjClosure* closure = newClosure(function);
    pop();
    push(OBJ_VAL(closure));
    call(closure, 0);

    return run();
}
