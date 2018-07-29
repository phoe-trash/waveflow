<p align="center">
  <img src="doc/logo.png">
</p>

# Waveflow - task orchestration library for Common Lisp

## Summary

Waveflow is a task orchestration library for Common Lisp. It is a framework
for expressing complex workflows by means of waves and flows.

Waveflow exposes several default subtypes of waves and flows for general usage.
You can access them by loading the `WAVEFLOW` ASDF system and using the
`WAVEFLOW` package.

The `WAVEFLOW/BT` subsystem exposes asynchronous versions of the `WAVEFLOW`
waves and flows. The asynchronous waves will execute separately, each in its own
thread. This means that waves that do not depend on each other will execute
concurrently.

## Exports

### Variables

  * **Variable `*WAVES*`**

    Its value is a hash-table mapping from symbols naming the waves to the wave
    objects themselves.

    By default, the value of `*WAVES*` is an empty hash table.

  * **Variable `*FLOWS*`**

    Its value is a hash-table mapping from symbols naming the flows to the flow
    objects themselves.

    By default, the value of `*WAVES*` is an empty hash table.

### Classes and Condition Types

  * **Class `WAVE`**

    A wave is an object representing a separate, independent part of the
    computation process. Waves can belong to different classes, with each class
    having its own means of execution.

    The `WAVE` class is the superclass for all waves and implements basic
    logging functionality that can be reimplemented by subclasses.

    Accessors:
    * **Reader `NAME`** - returns the symbol that names the wave.
    * **Accessor `DESCRIPTION`** - returns the string describing the wave.

  * **Condition Type `WAVE-FAILURE` (`ERROR`)**

    A wave failure is an error that is signaled whenever wave execution is
    unsuccessful.

    Readers:
    * **Reader `WAVE`** - returns the wave whose execution has failed.
    * **Reader `REASON`** - returns the original error that triggered the wave
      failure.

  * **Class `CALLBACK-WAVE` (`WAVE`)**

    Represents a wave whose execution is facilitated by calling a callback
    function stored on the wave instance.

    Accessors:
    * **Accessor `CALLBACK`** - accesses the wave's callback. Its value must be
      a function whose protocol matches that of the generic function
      `EXECUTE-WAVE`.

  * **Class `RETRY-WAVE` (`CALLBACK-WAVE`)**

    Represents a callback wave whose callback may need to be retried in order to
    succeed. The callback will be called up to a specified number of times, with
    the specified retry function being called between each callback call.

    Accessors:
    * **Accessor `RETRY-FN`** - accesses the wave's retry function. Its value
      must be a function whose protocol matches that of the generic function
      `EXECUTE-WAVE`.
    * **Accessor `RETRY-COUNT`** - accesses the wave's retry count. Its value
      must be a function whose protocol matches that of the generic function
      `EXECUTE-WAVE`.

  * **Condition Type `RETRY-WAVE-FAILURE` (`WAVE-FAILURE`)**

    A condition type representing failure of a retry wave.

  * **Class `WRAPPED-WAVE` (`WAVE`)**

    Represents a wave whose execution requires code to be executed as a prologue
    (before-function) and an epilogue (after-function) of the function's main
    body.

    Accessors:
    * **Accessor `BEFORE-FN`** - accesses the wave's before-function. Its value
      must be a function whose lambda list matches that of the generic function
      `EXECUTE-WAVE`. Its secondary return value is ignored.
    * **Accessor `AFTER-FN`** - accesses the wave's after-function. Its value
      must be a function whose lambda list matches that of the generic function
      `EXECUTE-WAVE`. Its secondary return value is ignored.

  * **Condition Type `WRAPPED-WAVE-FAILURE` (`WAVE-FAILURE`)**

    A condition type representing failure of a wrapped wave.

    Readers:
    * **Reader `STATE`** - one of `:BEFORE`, `:DURING`, or `:AFTER`. Denotes
      if the failure occurred during the execution of the before-function,
      the proper method call, or the after-function.
    * **Reader `BEFORE-RESULT`** - if `STATE` is one of `:DURING` or `:AFTER`,
      returns the value of the before-function. Otherwise, returns `NIL`.
    * **Reader `DURING-RESULT`** - if `STATE` is `:AFTER`, returns the value of
    the method call. Otherwise, returns `NIL`.

  * **Class `PUSH-WAVE` (`WRAPPED-WAVE`)**

    Represents a wrapped wave that is meant to facilitate moving data from a
    local source (retrieved by means of a load function) to a remote source
    (pushed by means of a push function). The push function is expected to
    access the network.

    Accessors:
    * **Accessor `LOAD-FN`** - accesses the wave's load function. Its value
      must be a function whose protocol matches that of the generic function
      `EXECUTE-WAVE`. Its secondary return value is passed to the wave's push
      function.
    * **Accessor `PUSH-FN`** - accesses the wave's push function. Its value
      must be a function whose protocol matches that of the generic function
      `EXECUTE-WAVE`. Its secondary return value is ignored.

  * **Class `PULL-WAVE` (`WRAPPED-WAVE`)**

    Represents a wrapped wave that is meant to facilitate moving data from a
    remote source (retrieved by means of a pull function) to a local source
    (pushed by means of a save function). The pull function is expected to
    access the network whereas the save function is expected to cause side
    effects (such as modifying other objects or writing to disk).

    Accessors:
    * **Accessor `PULL-FN`** - accesses the wave's pull function. Its value
      must be a function whose protocol matches that of the generic function
      `EXECUTE-WAVE`. Its secondary return value is passed to the wave's save
      function.
    * **Accessor `SAVE-FN`** - accesses the wave's save function. Its value
      must be a function whose protocol matches that of the generic function
      `EXECUTE-WAVE`. Its secondary return value is ignored.

  * **Class `FLOW`**

    A flow is an object representing a whole workflow, understood as a
    collection of waves and dependencies between them, as well as metadata about
    how they should be executed.

    Accessors:
    * **Reader `NAME`**
    * **Accessor `SPAWN-FN`**
    * **Acccessor `WAVES`**

### Macros

  * **Macro `DEFINE-WAVE`**

  * **Macro `DEFINE-FLOW`**

### Generic Functions

  * **Generic Function `LOGGER`**

  * **Generic Function `EXECUTE-WAVE`**

  * **Generic Function `EXECUTE-FLOW`**

## Example

## Extending Waveflow
