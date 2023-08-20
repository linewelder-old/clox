#include <math.h>
#include <stdio.h>
#include <time.h>

#include "vm.h"

static Value clockNative(Value* args) {
    return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

static Value readNumberNative(Value* args) {
    double result = NAN;
    scanf("%lf", &result);

    int c;
    while ((c = getchar()) != '\n' && c != EOF);

    if (isnan(result)) return NIL_VAL;
    return NUMBER_VAL(result);
}

static Value errorNative(Value* args) {
    if (!IS_STRING(args[0])) {
        runtimeError("Argument must be a string.");
        return NIL_VAL;
    }

    runtimeError("Runtime error: %s", AS_CSTRING(args[0]));
    return NIL_VAL;
}

static Value heapSizeNative(Value* args) {
    return NUMBER_VAL(vm.bytesAllocated);
}

static Value setPropertyNative(Value* args) {
    if (!IS_INSTANCE(args[0])) {
        runtimeError("First argument must be a class instance.");
        return NIL_VAL;
    }

    if (!IS_STRING(args[1])) {
        runtimeError("Second argument must be a string.");
        return NIL_VAL;
    }

    tableSet(&AS_INSTANCE(args[0])->fields,
             AS_STRING(args[1]),
             args[2]);
    return args[2];
}

static Value getPropertyNative(Value* args) {
    if (!IS_INSTANCE(args[0])) {
        runtimeError("First argument must be a class instance.");
        return NIL_VAL;
    }

    if (!IS_STRING(args[1])) {
        runtimeError("Second argument must be a string.");
        return NIL_VAL;
    }

    Value value;
    if (!tableGet(&AS_INSTANCE(args[0])->fields,
                  AS_STRING(args[1]),
                  &value)) {
        runtimeError("Undefined property '%s'.", AS_CSTRING(args[1]));
        return NIL_VAL;
    }

    return value;
}

static Value hasPropertyNative(Value* args) {
    if (!IS_INSTANCE(args[0])) {
        runtimeError("First argument must be a class instance.");
        return NIL_VAL;
    }

    if (!IS_STRING(args[1])) {
        runtimeError("Second argument must be a string.");
        return NIL_VAL;
    }

    return BOOL_VAL(tableContains(
                        &AS_INSTANCE(args[0])->fields,
                        AS_STRING(args[1])));
}

static Value deletePropertyNative(Value* args) {
    if (!IS_INSTANCE(args[0])) {
        runtimeError("First argument must be a class instance.");
        return NIL_VAL;
    }

    if (!IS_STRING(args[1])) {
        runtimeError("Second argument must be a string.");
        return NIL_VAL;
    }

    tableDelete(&AS_INSTANCE(args[0])->fields,
                AS_STRING(args[1]));
    return NIL_VAL;
}

void initStdlib() {
    defineNative("clock", clockNative, 0);
    defineNative("readNumber", readNumberNative, 0);
    defineNative("error", errorNative, 1);
    defineNative("heapSize", heapSizeNative, 0);
    defineNative("setProperty", setPropertyNative, 3);
    defineNative("getProperty", getPropertyNative, 2);
    defineNative("hasProperty", hasPropertyNative, 2);
    defineNative("deleteProperty", deletePropertyNative, 2);
}
