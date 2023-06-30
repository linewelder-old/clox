#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

typedef enum {
    OP_CONSTANT,
    OP_RETURN,
} OpCode;

typedef struct {
    int line;
    int length;
} LineEntry;

typedef struct {
    int count;
    int capacity;
    uint8_t* code;

    int linesCount;
    int linesCapacity;
    LineEntry* lines;

    ValueArray constants;
} Chunk;

void initChunk(Chunk* chunk);
void freeChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte, int line);
int getLine(Chunk* chunk, int offset);
int addConstant(Chunk* chunk, Value value);

#endif
