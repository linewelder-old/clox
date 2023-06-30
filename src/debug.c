#include <stdio.h>

#include "debug.h"
#include "value.h"

static int simpleInstruction(const char* name, int offset) {
    printf("%s\n", name);
    return offset + 1;
}

static int constantInstruction(const char* name, Chunk* chunk, int offset) {
    uint8_t constant = chunk->code[offset + 1];
    printf("%-16s %d '", name, constant);
    printValue(chunk->constants.values[constant]);
    printf("'\n");
    return offset + 2;
}

static int constantLongInstruction(const char* name, Chunk* chunk, int offset) {
    int constant = chunk->code[offset + 1] |
                   chunk->code[offset + 2] << 8 |
                   chunk->code[offset + 3] << 16;
    printf("%-16s %d '", name, constant);
    printValue(chunk->constants.values[constant]);
    printf("'\n");
    return offset + 4;
}

// line = -1 means the line is the same as for the previous instruction.
static int disassembleInstruction_(Chunk* chunk, int offset, int line) {
    printf("%04d ", offset);
    if (line < 0) {
        printf("   | ");
    } else {
        printf("%4d ", line);
    }

    uint8_t instruction = chunk->code[offset];
    switch (instruction) {
        case OP_CONSTANT:
            return constantInstruction("OP_CONSTANT", chunk, offset);
        case OP_CONSTANT_LONG:
            return constantLongInstruction("OP_CONSTANT_LONG", chunk, offset);
        case OP_ADD:
            return simpleInstruction("OP_ADD", offset);
        case OP_SUBTRACT:
            return simpleInstruction("OP_SUBTRACT", offset);
        case OP_MULTIPLY:
            return simpleInstruction("OP_MULTIPLY", offset);
        case OP_DIVIDE:
            return simpleInstruction("OP_DIVIDE", offset);
        case OP_NEGATE:
            return simpleInstruction("OP_NEGATE", offset);
        case OP_RETURN:
            return simpleInstruction("OP_RETURN", offset);
        default:
            printf("Unknown opcode %d\n", instruction);
            return offset + 1;
    }
}

int disassembleInstruction(Chunk* chunk, int offset) {
    disassembleInstruction_(chunk, offset, getLine(chunk, offset));
}

void disassembleChunk(Chunk* chunk, const char* name) {
    printf("== %s ==\n", name);
    if (chunk->count == 0) return;

    LineEntry* currentLinePtr = chunk->lines;
    int currentLine = currentLinePtr->line;
    int endOffset = currentLinePtr->length;

    for (int offset = 0; offset < chunk->count;) {
        offset = disassembleInstruction_(chunk, offset, currentLine);
        currentLine = -1;

        if (offset == endOffset) {
            currentLinePtr++;
            currentLine = currentLinePtr->line;
            endOffset += currentLinePtr->length;
        }
    }
}
