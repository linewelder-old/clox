#include <stdio.h>

#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "memory.h"
#include "vm.h"

VM vm;

static void resetStack() {
    vm.stackTop = vm.stack;
    vm.stackEnd = vm.stack;
}

void initVM() {
    resetStack();
}

void freeVM() {
    FREE_ARRAY(Value, vm.stack, vm.stackEnd - vm.stack);
}

void push(Value value) {
    if (vm.stackEnd <= vm.stackTop) {
        int stackSize = vm.stackTop - vm.stack;
        int oldCapacity = vm.stackEnd - vm.stack;

        int newCapacity = GROW_CAPACITY(oldCapacity);
        vm.stack = GROW_ARRAY(Value, vm.stack, oldCapacity, newCapacity);
        vm.stackTop = vm.stack + stackSize;
        vm.stackEnd = vm.stack + newCapacity;
    }

    *vm.stackTop = value;
    vm.stackTop++;
}

Value pop() {
    vm.stackTop--;
    return *vm.stackTop;
}

static InterpretResult run() {
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
#define READ_CONSTANT_LONG() \
    (vm.chunk->constants.values[ \
        READ_BYTE() | (READ_BYTE() << 8) | (READ_BYTE() << 16) \
    ])
#define BINARY_OP(op) \
    do { \
        vm.stackTop--; \
        double b = vm.stackTop[1]; \
        double a = vm.stackTop[0]; \
        *vm.stackTop = a op b; \
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
        disassembleInstruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
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
            case OP_ADD:      BINARY_OP(+); break;
            case OP_SUBTRACT: BINARY_OP(-); break;
            case OP_MULTIPLY: BINARY_OP(*); break;
            case OP_DIVIDE:   BINARY_OP(/); break;
            case OP_NEGATE:   *vm.stackTop = -*vm.stackTop; break;
            case OP_RETURN: {
                printValue(pop());
                printf("\n");
                return INTERPRET_OK;
            }
        }
    }

#undef READ_BYTE
#undef READ_CONSTANT
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
    Chunk chunk;
    initChunk(&chunk);

    if (!compile(source, &chunk)) {
        freeChunk(&chunk);
        return INTERPRET_COMPILE_ERROR;
    }

    vm.chunk = &chunk;
    vm.ip = vm.chunk->code;
    InterpretResult result = run();

    freeChunk(&chunk);
    return result;
}
