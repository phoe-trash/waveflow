<p align="center">
  <img src="doc/logo.png">
</p>

# Waveflow - task orchestration library for Common Lisp

## Summary

Waveflow is a task orchestration library for Common Lisp. It is a framework
for expressing complex workflows by means of waves and flows.

A wave is an object representing a separate, independent part of the computation
process.

A flow is an object representing a whole workflow, understood as a collection of
waves and dependencies between them.

Waveflow exposes several default subtypes of waves and flows for general usage.
You can access them by loading the `WAVEFLOW` ASDF system and using the
`WAVEFLOW` package.

The `WAVEFLOW/BT` subsystem exposes asynchronous versions of the `WAVEFLOW`
waves and flows. The asynchronous waves will execute separately, each in its own
thread. This means that waves that do not depend on each other will execute
concurrently.

## Useful Classes

## Exports

## Example

## Extending Waveflow
