#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
    const char* source;
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
} Parser;

typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT,  // =
    PREC_OR,          // or
    PREC_AND,         // and
    PREC_EQUALITY,    // == !=
    PREC_COMPARISON,  // < > <= >=
    PREC_TERM,        // + -
    PREC_FACTOR,      // * /
    PREC_UNARY,       // ! -
    PREC_CALL,        // . ()
    PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(bool canAssign);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

typedef struct {
    Token name;
    int depth;
} Local;

typedef struct {
    Local* locals;
    int localCount;
    int localCapacity;
    int scopeDepth;

    int loopContinueOffset; // -1 if not in a loop.
} Compiler;

Parser parser;
Compiler* current = NULL;
Chunk* compilingChunk;

static Chunk* currentChunk() {
    return compilingChunk;
}

static const char* findLine(int line) {
    const char* start = parser.source;
    int currentLine = 1;
    while (currentLine < line && *start != '\0') {
        if (*start == '\n') currentLine++;
        start++;
    }

    return start;
}

static void errorAt(Token* token, const char* message) {
    if (parser.panicMode) return;
    parser.panicMode = true;
    fprintf(stderr, "[line %d] Error", token ->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);

    const char* lineStart = findLine(token->line);
    fprintf(stderr, "%3d | ", token->line);
    for (const char* ch = lineStart; *ch != '\0' && *ch != '\n'; ch++) {
        putc(*ch, stderr);
    }
    putc('\n', stderr);

    int offset = token->start - lineStart;
    fprintf(stderr, "    | ");
    for (int i = 0; i < offset; i++) {
        putc(' ', stderr);
    }
    for (int i = 0; i < token->length; i++) {
        putc('~', stderr);
    }
    putc('\n', stderr);

    parser.hadError = true;
}

static void error(const char* message) {
    errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char* message) {
    errorAt(&parser.current, message);
}

static void advance() {
    parser.previous = parser.current;

    for (;;) {
        parser.current = scanToken();
        if (parser.current.type != TOKEN_ERROR) break;

        errorAtCurrent(parser.current.errorMessage);
    }
}

static void consume(TokenType type, const char* message) {
    if (parser.current.type == type) {
        advance();
        return;
    }

    errorAtCurrent(message);
}

static bool check(TokenType type) {
    return parser.current.type == type;
}

static bool match(TokenType type) {
    if (!check(type)) return false;
    advance();
    return true;
}

static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitLong(int value) {
    writeLong(currentChunk(), value, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

static void emitLoop(int loopStart) {
    emitByte(OP_LOOP);

    // +2 to account for the offset itself.
    int offset = currentChunk()->count - loopStart + 2;
    if (offset > UINT16_MAX) error("Loop body too large.");

    emitByte(offset & 0xff);
    emitByte((offset >> 8) & 0xff);
}

static int emitJump(uint8_t instruction) {
    emitByte(instruction);
    emitByte(0xff);
    emitByte(0xff);
    return currentChunk()->count - 2;
}

static void emitReturn() {
    emitByte(OP_RETURN);
}

static int makeConstant(Value value) {
    int constant = addConstant(currentChunk(), value);
    if (constant> CONSTANT_ID_MAX) {
        error("Too many constants in one chunk.");
    }
}

static void emitConstant(Value value) {
    int constant = writeConstant(currentChunk(), value, parser.previous.line);
    if (constant > CONSTANT_ID_MAX) {
        error("Too many constants in one chunk.");
    }
}

static void patchJump(int offset) {
    // -2 to account for the jump offset itself.
    int jump = currentChunk()->count - offset - 2;
    if (jump > UINT16_MAX) {
        error("Too much code to jump over.");
    }

    currentChunk()->code[offset] = jump & 0xff;
    currentChunk()->code[offset + 1] = (jump >> 8) & 0xff;
}

static void initCompiler(Compiler* compiler) {
    compiler->locals = NULL;
    compiler->localCount = 0;
    compiler->localCapacity = 0;
    compiler->scopeDepth = 0;

    compiler->loopContinueOffset = -1;
    current = compiler;
}

static void endCompiler() {
    emitReturn();
    FREE_ARRAY(Local, current->locals, current->localCapacity);
#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError) {
        disassembleChunk(currentChunk(), "code");
    }
#endif
}

static void beginScope() {
    current->scopeDepth++;
}

static void endScope() {
    current->scopeDepth--;

    while (current->localCount > 0 &&
           current->locals[current->localCount - 1].depth >
               current->scopeDepth) {
        emitByte(OP_POP);
        current->localCount--;
    }
}

static void expression();
static void statement();
static void declaration();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);

static int identifierConstant(Token* name) {
    return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

static bool identifiersEqual(Token* a, Token* b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler* compiler, Token* name) {
    for (int i = compiler->localCount - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if (identifiersEqual(name, &local->name)) {
            if (local->depth == -1) {
                error("Can't read local variable in its own initializer.");
            }
            return i;
        }
    }

    return -1;
}

static void addLocal(Token name) {
    if (current->localCount == MAX_LOCALS) {
        error("Too many local variables in function.");
        return;
    }

    if (current->localCapacity < current->localCount + 1) {
        int oldCapacity = current->localCapacity;
        current->localCapacity = GROW_CAPACITY(oldCapacity);
        current->locals = GROW_ARRAY(Local, current->locals,
                                     oldCapacity, current->localCapacity);
    }

    Local* local = &current->locals[current->localCount++];
    local->name = name;
    local->depth = -1;
}

static void declareVariable() {
    if (current->scopeDepth == 0) return;

    Token* name = &parser.previous;
    for (int i = current->localCount - 1; i >= 0; i--) {
        Local* local = &current->locals[i];
        if (local->depth != -1 && local->depth < current->scopeDepth) {
            break;
        }

        if (identifiersEqual(name, &local->name)) {
            error("Already a variable with this name in this scope.");
        }
    }

    addLocal(*name);
}

static void binary(bool canAssign) {
    TokenType operatorType = parser.previous.type;
    ParseRule* rule = getRule(operatorType);
    parsePrecedence((Precedence)(rule->precedence + 1));
    
    switch (operatorType) {
        case TOKEN_PLUS:          emitByte(OP_ADD); break;
        case TOKEN_MINUS:         emitByte(OP_SUBTRACT); break;
        case TOKEN_STAR:          emitByte(OP_MULTIPLY); break;
        case TOKEN_SLASH:         emitByte(OP_DIVIDE); break;
        case TOKEN_BANG_EQUAL:    emitBytes(OP_EQUAL, OP_NOT); break;
        case TOKEN_EQUAL_EQUAL:   emitByte(OP_EQUAL); break;
        case TOKEN_GREATER:       emitByte(OP_GREATER); break;
        case TOKEN_GREATER_EQUAL: emitBytes(OP_LESS, OP_NOT); break;
        case TOKEN_LESS:          emitByte(OP_LESS); break;
        case TOKEN_LESS_EQUAL:    emitBytes(OP_GREATER, OP_NOT); break;
        default: return; // Unreachable.
    }
}

static void and_(bool canAssign) {
    int endJump = emitJump(OP_JUMP_IF_FALSE);

    emitByte(OP_POP);
    parsePrecedence(PREC_AND);

    patchJump(endJump);
}

static void or_(bool canAssign) {
    int elseJump = emitJump(OP_JUMP_IF_FALSE);
    int endJump = emitJump(OP_JUMP);

    patchJump(elseJump);
    emitByte(OP_POP);

    parsePrecedence(PREC_OR);
    patchJump(endJump);
}

static void literal(bool canAssign) {
    switch (parser.previous.type) {
        case TOKEN_FALSE: emitByte(OP_FALSE); break;
        case TOKEN_NIL:   emitByte(OP_NIL); break;
        case TOKEN_TRUE:  emitByte(OP_TRUE); break;
        default: return; // Unreachable.
    }
}

static void grouping(bool canAssign) {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number(bool canAssign) {
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

static void string(bool canAssign) {
    ObjString* str = copyString(parser.previous.start + 1,
                                parser.previous.length - 2);
    emitConstant(OBJ_VAL(str));
}

static void namedVariable(Token name, bool canAssign) {
    uint8_t getOp, setOp;
    int arg = resolveLocal(current, &name);

    bool isAssignment = canAssign && match(TOKEN_EQUAL);
    if (isAssignment) expression();

    bool isGlobal = arg == -1;
    if (isGlobal) arg = identifierConstant(&name);

    bool isLong = arg >= 256;
    if (isGlobal) {
        if (isLong) {
            getOp = OP_GET_GLOBL_LNG;
            setOp = OP_SET_GLOBL_LNG;
        } else {
            getOp = OP_GET_GLOBAL;
            setOp = OP_SET_GLOBAL;
        }
    } else {
        if (isLong) {
            getOp = OP_GET_LOCAL_LNG;
            setOp = OP_SET_LOCAL_LNG;
        } else {
            getOp = OP_GET_LOCAL;
            setOp = OP_SET_LOCAL;
        }
    }

    emitByte(isAssignment ? setOp : getOp);
    if (isLong) {
        emitLong(arg);
    } else {
        emitByte((uint8_t)arg);
    }
}

static void variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}

static void unary(bool canAssign) {
    TokenType operatorType = parser.previous.type;

    parsePrecedence(PREC_UNARY);

    switch (operatorType) {
        case TOKEN_BANG:  emitByte(OP_NOT); break;
        case TOKEN_MINUS: emitByte(OP_NEGATE); break;
        default: return; // Unreachable.
    }
}

ParseRule rules[] = {
    [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
    [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
    [TOKEN_OR]            = {NULL,     or_,    PREC_OR},
    [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
    [TOKEN_LEFT_PAREN]    = {grouping, NULL,   PREC_NONE},
    [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
    [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
    [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
    [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
    [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE},
    [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},
    [TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY},
    [TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE},
    [TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
    [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
    [TOKEN_AND]           = {NULL,     and_,   PREC_AND},
    [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
};

static void parsePrecedence(Precedence precedence) {
    advance();
    ParseFn prefixRule = getRule(parser.previous.type)->prefix;
    if (prefixRule == NULL) {
        error("Expect expression.");
        return;
    }

    bool canAssign = precedence <= PREC_ASSIGNMENT;
    prefixRule(canAssign);

    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule(canAssign);
    }

    if (canAssign && match(TOKEN_EQUAL)) {
        error("Invalid assignment target.");
    }
}

static int parseVariable(const char* errorMessage) {
    consume(TOKEN_IDENTIFIER, errorMessage);

    declareVariable();
    if (current->scopeDepth > 0) return 0;

    return identifierConstant(&parser.previous);
}

static void markInitialized() {
    current->locals[current->localCount - 1].depth = current->scopeDepth;
}

static void defineVariable(int global) {
    if (current->scopeDepth > 0) {
        markInitialized();
        return;
    }

    if (global < 256) {
        emitBytes(OP_DEFINE_GLOBAL, (uint8_t)global);
    } else {
        emitByte(OP_DEF_GLOBL_LNG);
        emitLong(global);
    }
}

static ParseRule* getRule(TokenType type) {
    return &rules[type];
}

static void expression() {
    parsePrecedence(PREC_ASSIGNMENT);
}

static void block() {
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void varDeclaration() {
    int global = parseVariable("Expect variable name.");

    if (match(TOKEN_EQUAL)) {
        expression();
    } else {
        emitByte(OP_NIL);
    }

    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");
    defineVariable(global);
}

static void expressionStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    emitByte(OP_POP);
}

static void continueStatement() {
    int jumpOffset = current->loopContinueOffset;
    if (jumpOffset < 0) {
        error("Can't continue outside loop.");
    }

    emitLoop(jumpOffset);
    consume(TOKEN_SEMICOLON, "Expect ';' after 'continue'.");
}

static void forStatement() {
    beginScope();
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
    if (match(TOKEN_SEMICOLON)) {
        // No initializer.
    } else if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        expressionStatement();
    }

    int loopStart = currentChunk()->count;
    int exitJump = -1;
    if (!match(TOKEN_SEMICOLON)) {
        expression();
        consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

        exitJump = emitJump(OP_JUMP_IF_FALSE);
        emitByte(OP_POP);
    }

    if (!match(TOKEN_RIGHT_PAREN)) {
        int bodyJump = emitJump(OP_JUMP);
        int incrementStart = currentChunk()->count;
        expression();
        emitByte(OP_POP);
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

        emitLoop(loopStart);
        loopStart = incrementStart;
        patchJump(bodyJump);
    }

    int previousContinueOffset = current->loopContinueOffset;
    current->loopContinueOffset = loopStart;
    statement();
    current->loopContinueOffset = previousContinueOffset;
    emitLoop(loopStart);

    if (exitJump != -1) {
        patchJump(exitJump);
        emitByte(OP_POP);
    }
    endScope();
}

static void ifStatement() {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int thenJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);
    statement();

    int elseJump = emitJump(OP_JUMP);
    patchJump(thenJump);
    emitByte(OP_POP);

    if (match(TOKEN_ELSE)) statement();
    patchJump(elseJump);
}

static void printStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_PRINT);
}

static void switchStatement() {
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'switch'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int skipExitJump = emitJump(OP_JUMP);
    int exitJumpLocation = currentChunk()->count;
    int exitJump = emitJump(OP_JUMP);
    patchJump(skipExitJump);

    consume(TOKEN_LEFT_BRACE, "Expect '{' before switch body.");

    if (!check(TOKEN_CASE) && !check(TOKEN_DEFAULT) &&
        !check(TOKEN_RIGHT_BRACE)) {
        errorAtCurrent("Expect 'case' or 'default' before any statements.");
    }

    int skipJump = -1;
    bool inDefault = false;
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        if (match(TOKEN_CASE)) {
            if (inDefault) {
                error("Default case must be the last one.");
            }

            if (skipJump >= 0) {
                emitLoop(exitJumpLocation);
                patchJump(skipJump);
                emitByte(OP_POP);
            }

            emitByte(OP_DUPLICATE);
            expression();
            emitByte(OP_EQUAL);

            skipJump = emitJump(OP_JUMP_IF_FALSE);
            emitByte(OP_POP);
            consume(TOKEN_COLON, "Expect ':' after case expression.");
        } else if (match(TOKEN_DEFAULT)) {
            if (inDefault) {
                error("Multiple default cases.");
            }

            if (skipJump >= 0) {
                emitLoop(exitJumpLocation);
                patchJump(skipJump);
                emitByte(OP_POP);

                skipJump = -1;
            }

            consume(TOKEN_COLON, "Expect ':' after 'default'.");
            inDefault = true;
        } else {
            statement();
        }
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after switch body.");

    if (skipJump >= 0) {
        patchJump(skipJump);
        emitByte(OP_POP);
    }
    patchJump(exitJump);
    emitByte(OP_POP);
}

static void whileStatement() {
    int loopStart = currentChunk()->count;
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

    int exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP);

    int previousContinueOffset = current->loopContinueOffset;
    current->loopContinueOffset = loopStart;
    statement();
    current->loopContinueOffset = previousContinueOffset;
    emitLoop(loopStart);

    patchJump(exitJump);
    emitByte(OP_POP);
}

static void synchronize() {
    parser.panicMode = false;

    while (parser.current.type != TOKEN_EOF) {
        if (parser.previous.type == TOKEN_SEMICOLON) return;
        switch (parser.current.type) {
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_PRINT:
            case TOKEN_RETURN:
                return;
        }
        advance();
    }
}

static void statement() {
    if (match(TOKEN_PRINT)) {
        printStatement();
    } else if (match(TOKEN_CONTINUE)) {
        continueStatement();
    } else if (match(TOKEN_FOR)) {
        forStatement();
    } else if (match(TOKEN_IF)) {
        ifStatement();
    } else if (match(TOKEN_SWITCH)) {
        switchStatement();
    } else if (match(TOKEN_WHILE)) {
        whileStatement();
    } else if (match(TOKEN_LEFT_BRACE)) {
        beginScope();
        block();
        endScope();
    } else {
        expressionStatement();
    }
}

static void declaration() {
    if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        statement();
    }

    if (parser.panicMode) synchronize();
}

bool compile(const char* source, Chunk* chunk) {
    parser.source = source;
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler);
    compilingChunk = chunk;

    parser.hadError = false;
    parser.panicMode = false;

    advance();

    while (!match(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_EOF, "Expect end of expression.");
    endCompiler();
    return !parser.hadError;
}
