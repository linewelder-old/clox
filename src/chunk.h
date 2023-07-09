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
    OP_POP,
    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_GET_LOCAL_LNG,
    OP_SET_LOCAL_LNG,
    OP_GET_GLOBAL,
    OP_DEFINE_GLOBAL,
    OP_SET_GLOBAL,
    OP_GET_GLOBL_LNG,
    OP_DEF_GLOBL_LNG,
    OP_SET_GLOBL_LNG,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_NOT,
    OP_NEGATE,
    OP_PRINT,
    OP_JUMP,
    OP_JUMP_IF_FALSE,
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
void writeLong(Chunk* chunk, int value, int line);

int getLine(Chunk* chunk, int offset);

int addConstant(Chunk* chunk, Value value);
int writeConstant(Chunk* chunk, Value value, int line);

#endif
