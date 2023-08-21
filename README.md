# clox

A brand new Lox interpreter, now in C, still written along with Robert Nystrom's
wonderful book ["Crafting Interpreters"](http://craftinginterpreters.com).

The test suite was adapted from ["the book's
repository"](https://github.com/munificent/craftinginterpreters).

## Differences from the original version

The interpreter supports features that were suggested to be added in the
Challenges sections. This section only documents the differences that affect
the language.

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

- [Several new built-in functions](doc/builtin.md)
