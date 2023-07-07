# clox

A brand new Lox interpreter, now in C, still written along with Robert Nystrom's
wonderful book ["Crafting Interpreters"](http://craftinginterpreters.com).

## Differences from the original version

The interpreter supports features that were suggested to be added in the
Challenges sections. This section only documents the differences that affect
the language.

- Up to 2^24 constants in a code chunk

  Every literal or global variable reference produces a constant at compile
  time. The original clox from the book only supports up to 256 constants per
  chunk, a chunk being for example a function. This implentation on the other
  hand supports up to 2^24 of them.

- Infinite stack capacity

  The stack grows to accomodate all the values pushed to it.

  __TODO: when functions are added, add checks to prevent infinite recursion.__
