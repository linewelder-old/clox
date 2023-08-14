#include <stdlib.h>

#include "chunk.h"
#include "memory.h"
#include "value.h"
#include "vm.h"

void initChunk(Chunk* chunk) {
    chunk->count = 0;
    chunk->capacity = 0;
    chunk->code = NULL;

    chunk->linesCount = 0;
    chunk->linesCapacity = 0;
    chunk->lines = NULL;

    initValueArray(&chunk->constants);
}

void freeChunk(Chunk* chunk) {
    FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
    FREE_ARRAY(LineEntry, chunk->lines, chunk->linesCapacity);
    freeValueArray(&chunk->constants);
    initChunk(chunk);
}

void writeChunk(Chunk* chunk, uint8_t byte, int line) {
    if (chunk->capacity < chunk->count + 1) {
        int oldCapacity = chunk->capacity;
        chunk->capacity = GROW_CAPACITY(oldCapacity);
        chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
    }

    chunk->code[chunk->count] = byte;
    chunk->count++;

    if (chunk->linesCount > 0 && chunk->lines[chunk->linesCount - 1].line == line) {
        chunk->lines[chunk->linesCount - 1].length++;
        return;
    }

    if (chunk->linesCapacity < chunk->linesCount + 1) {
        int oldCapacity = chunk->linesCapacity;
        chunk->linesCapacity = GROW_CAPACITY(oldCapacity);
        chunk->lines = GROW_ARRAY(
            LineEntry, chunk->lines, oldCapacity, chunk->linesCapacity);
    }

    chunk->lines[chunk->linesCount].line = line;
    chunk->lines[chunk->linesCount].length = 1;
    chunk->linesCount++;
}

int addConstant(Chunk* chunk, Value value) {
    push(value);
    writeValueArray(&chunk->constants, value);
    pop();
    return chunk->constants.count - 1;
}

int getLine(Chunk* chunk, int offset) {
    int currentOffset = 0;
    for (int i = 0; i < chunk->linesCount; i++) {
        currentOffset += chunk->lines[i].length;
        if (currentOffset > offset) {
            return chunk->lines[i].line;
        }
    }

    return -1;
}

void writeLong(Chunk* chunk, int value, int line) {
    writeChunk(chunk, value & 0x0000FF, line);
    writeChunk(chunk, (value & 0x00FF00) >> 8, line);
    writeChunk(chunk, (value & 0xFF0000) >> 16, line);
}

int writeConstant(Chunk* chunk, Value value, int line) {
    int constant = addConstant(chunk, value);
    if (constant < 256) {
        writeChunk(chunk, OP_CONSTANT, line);
        writeChunk(chunk, constant, line);
    } else {
        writeChunk(chunk, OP_CONSTANT_LONG, line);
        writeLong(chunk, constant, line);
    }

    return constant;
}
