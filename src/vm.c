#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "memory.h"
#include "object.h"
#include "vm.h"

VM vm;

static void resetStack() {
    vm.stackTop = vm.stack;
    vm.stackEnd = vm.stack;
    vm.frameCount = 0;
}

static void runtimeError(const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    CallFrame* frame = &vm.frames[vm.frameCount - 1];
    size_t instruction = frame->ip - frame->function->chunk.code - 1;
    int line = getLine(&frame->function->chunk, instruction);
    fprintf(stderr, "[line %d] in script\n", line);
    resetStack();
}

void initVM() {
    resetStack();
    vm.objects = NULL;
    initTable(&vm.globals);
    initTable(&vm.strings);
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
        vm.stack = GROW_ARRAY(Value, vm.stack, oldCapacity, newCapacity);
        vm.stackTop = vm.stack + stackSize;
        vm.stackEnd = vm.stack + newCapacity;

        for (int i = 0; i < vm.frameCount; i++) {
            vm.frames[i].slots += vm.stack - oldStack;
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

static bool isFalsey(Value value) {
    return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void concatenate() {
    ObjString* b = AS_STRING(pop());
    ObjString* a = AS_STRING(pop());

    int length = a->length + b->length;
    char* chars = ALLOCATE(char, length + 1);
    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';

    ObjString* result = takeString(chars, length);
    push(OBJ_VAL(result));
}

static InterpretResult run() {
    CallFrame* frame = &vm.frames[vm.frameCount - 1];

#define READ_BYTE() (*frame->ip++)
#define READ_SHORT() \
    (frame->ip += 2, \
     (uint16_t)((frame->ip[-1] << 8) | frame->ip[-2]))
#define READ_LONG() \
    (frame->ip += 3, \
     (int)((frame->ip[-1] << 16) | (frame->ip[-2] << 8) | frame->ip[-3]))

#define READ_CONSTANT() (frame->function->chunk.constants.values[READ_BYTE()])
#define READ_CONSTANT_LONG() \
    (frame->function->chunk.constants.values[READ_LONG()])

#define READ_STRING() AS_STRING(READ_CONSTANT())
#define READ_STRING_LONG() AS_STRING(READ_CONSTANT_LONG())
#define BINARY_OP(valueType, op) \
    do { \
        Value valB = peek(0); \
        Value valA = peek(1); \
        if (!IS_NUMBER(valB) || !IS_NUMBER(valA)) { \
            runtimeError("Operands must be numbers."); \
            return INTERPRET_RUNTIME_ERROR; \
        } \
        double b = AS_NUMBER(valB); \
        double a = AS_NUMBER(valA); \
        vm.stackTop--; \
        vm.stackTop[-1] = valueType(a op b); \
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
        disassembleInstruction(&frame->function->chunk,
            (int)(frame->ip - frame->function->chunk.code));
#endif

        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
            case OP_CONSTANT: {
                Value constant = READ_CONSTANT();
                push(constant);
                break;
            }
            case OP_CONSTANT_LONG: {
                Value constant = READ_CONSTANT_LONG();
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
            case OP_GET_LOCAL_LNG: {
                int slot = READ_LONG();
                push(frame->slots[slot]);
                break;
            }
            case OP_SET_LOCAL_LNG: {
                int slot = READ_LONG();
                frame->slots[slot] = peek(0);
                break;
            }
#define GET_GLOBAL(name) \
    Value value; \
    if (!tableGet(&vm.globals, name, &value)) { \
        runtimeError("Undefined variable '%s'.", name->chars); \
        return INTERPRET_RUNTIME_ERROR; \
    } \
    push(value);
#define SET_GLOBAL(name) \
    if (tableSet(&vm.globals, name, peek(0))) { \
        tableDelete(&vm.globals, name); \
        runtimeError("Undefined variable '%s'.", name->chars); \
        return INTERPRET_RUNTIME_ERROR; \
    }
            case OP_GET_GLOBAL: {
                ObjString* name = READ_STRING();
                GET_GLOBAL(name);
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
                SET_GLOBAL(name);
                break;
            }
            case OP_GET_GLOBL_LNG: {
                ObjString* name = READ_STRING_LONG();
                GET_GLOBAL(name);
                break;
            }
            case OP_DEF_GLOBL_LNG: {
                ObjString* name = READ_STRING_LONG();
                tableSet(&vm.globals, name, peek(0));
                pop();
                break;
            }
            case OP_SET_GLOBL_LNG: {
                ObjString* name = READ_STRING_LONG();
                SET_GLOBAL(name);
                break;
            }
#undef GET_GLOBAL
#undef SET_GLOBAL
            case OP_EQUAL: {
                Value b = peek(0);
                Value a = peek(1);
                vm.stackTop--;
                vm.stackTop[-1] = BOOL_VAL(valuesEqual(a, b));
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
                    vm.stackTop[-1] = NUMBER_VAL(aNumber + bNumber);
                }
                break;
            }
            case OP_SUBTRACT: BINARY_OP(NUMBER_VAL, -); break;
            case OP_MULTIPLY: BINARY_OP(NUMBER_VAL, *); break;
            case OP_DIVIDE:   BINARY_OP(NUMBER_VAL, /); break;
            case OP_NOT:
                vm.stackTop[-1] = BOOL_VAL(isFalsey(peek(0)));
                break;
            case OP_NEGATE: {
                Value operand = peek(0);
                if (!IS_NUMBER(operand)) {
                    runtimeError("Operand must be a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }
                vm.stackTop[-1] = NUMBER_VAL(-AS_NUMBER(operand));
                break;
            }
            case OP_PRINT: {
                printValue(pop());
                printf("\n");
                break;
            }
            case OP_JUMP: {
                uint16_t offset = READ_SHORT();
                frame->ip += offset;
                break;
            }
            case OP_JUMP_IF_FALSE: {
                uint16_t offset = READ_SHORT();
                if (isFalsey(peek(0))) frame->ip += offset;
                break;
            }
            case OP_LOOP: {
                uint16_t offset = READ_SHORT();
                frame->ip -= offset;
                break;
            }
            case OP_RETURN: {
                return INTERPRET_OK;
            }
        }
    }

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

    CallFrame* frame = &vm.frames[vm.frameCount++];
    frame->function = function;
    frame->ip = function->chunk.code;
    frame->slots = vm.stack;

    return run();
}
