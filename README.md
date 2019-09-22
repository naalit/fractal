# Fractal

Fractal is a WIP programming language with a few goals:
- Run efficiently on both the CPU and GPU, via SPIR-V and Vulkan to work on all GPUs
- Use rich, statically verified pattern matching as a type system to enable more expressive types

## GPU Support

Fractal doesn't run on the GPU yet, but because it intends to, certain things are required:
- We need some way to know certain functions are pure, because you can't have side effects on the GPU
- We can't have garbage collection (or it needs to be optional), because you can't do that on the GPU
- We need to distinguish between heap-allocated and stack-allocated objects, because the GPU doesn't have a per-thread heap

The last two are (planned to be) addressed similar to Rust - types that represent owned heap pointers, garbage collected pointers, and also GPU-specific things like groupshared memory or uniform buffers. Like Rust, types can't be recursive without some sort of pointer in between, so recursive types can't exist on the GPU except for in groupshared memory or buffers - in which case the pointers would be stored as indices into the buffer.

## Patterns as types

In Fractal's core calculus, a function can take the form `term => term` instead of the traditional lambda-calculus `variable => term`. Function application is resolved via pattern matching coupled with substitution of free variables on the pattern side. The compiler can use a variation of normal pattern exhaustiveness checking to determine at compile time that a function will definitely be able to accept the value it's passed, and give a type error if it isn't guaranteed to match. Types (patterns) are sets of possible values, and are decoupled from the actual representation of a value.

## Current status

Currently, Fractal consists of a working parser and a tree-walking interpreter that can't interpret much of the AST produced by the parser.
