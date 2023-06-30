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
