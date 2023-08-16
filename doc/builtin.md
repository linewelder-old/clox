# Built-in functions

## `clock()`

Returns the number of seconds passed since the interpreter was launched.

This function was in the book.

## `readNumber()`

Reads a line from stdin, returns the first double it can read from it or nil if
fails.

## `error(message)`

Throws a runtime error with the given message. The message must be a string.

## `heapSize()`

Returns the total size in bytes of objects managed by the GC.

## `setProperty(instance, name, value)`

Assignes the value to the field with the given name (not neccessarily existing
one) on the class instance, returns the value. The name must be a string.

For example `setProperty(foo, "bar", true)` is equivalent to `foo.bar = true`.

## `getProperty(instance, name)`

Returns the value of the field with the given name on the class instance,
errors if the instance does not have it. The name must be a string.

For example `getProperty(foo, "bar")` is equivalent to `foo.bar`.

## `deleteProperty(instance, name)`

Deletes the field with the given name from the class instance. The name must be
a string.

## `hasProperty(instance, name)`

Returns true if the instance has a field with the given name, false otherwise.
The name must be a string.
