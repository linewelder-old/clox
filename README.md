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

- Up to 2^24 local variables in scope at the same time

  The original implementation only allows for up to 256.

- Infinite stack capacity

  The stack grows to accomodate all the values pushed to it.

- Switch statements

  The following code:

  ```javascript
  // Prints "Correct!".
  switch (2 + 2) {
      case 4:
          print "Correct!";
      case 5:
          print "Will not run";
          print 1 / 0;
      default:
          print "The default case";
  }
  ```

  Is semantically equivalent to this:

  ```javascript
  {
      var temp = 2 + 2;
      if (temp == 4) {
          print "Correct!";
      } else if (temp == 5) {
          print "Will not run";
          print 1 / 0;
      } else {
          print "The default case";
      }
  }
  ```

  _Note: unlike in most other languages, this switch statement allows for any
  expressions in `case` clauses, does not allow fallthroughs and thus does not
  require `break;`s at the end of each case._

- Continue statements

  Skips the rest of the loop body, jumping to the condition check for the next
  iteration.

  ```javascript
  // Prints only "4".
  for (var i = 0; i < 10; i = i + 1) {
      if (i != 4) continue;
      print i;
  }
  ```

- Arity checking for native function calls

  The number of arguments passed to a native function is checked.

  ```javascript
  print clock("extra argument"); // Error!
  ```

- New native functions!

  - `readNumber()` - reads a double from the user and returns it;
  - `error(message)` - throws a runtime error with the given string message;
