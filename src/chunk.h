#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

#define CONSTANT_ID_MAX 0xFFFFFF

typedef enum {
    OP_CONSTANT,
    OP_CONSTANT_LONG,
    OP_NIL,
    OP_TRUE,
    OP_FALSE,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_NEGATE,
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
int writeConstant(Chunk* chunk, Value value, int line);

#endif
