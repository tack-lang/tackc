# Architecture of `tackc`


```
Input -> Front-end -> SemA -> Middle-end -> Clover -> Back-end
```

## Front-end
The purpose of the front-end is to take in input files, and return a raw AST of the input.

## SemA
The objective of SemA, short for Semantic Analysis, is to take the raw AST from the front-end, and return a decorated AST. The information that is required from the decorated AST is yet to be decided, at time of writing.

## Middle-end
The job of the middle-end is to take the decorated AST, and turn it into Clover IR. The specification for Clover IR has not yet been written.

## Clover
Clover is an engine that can analyze an IR to ensure all contracts in the original Tack code are met. In addition to this, Clover can optimize the IR.

## Back-end
The purpose of the back-end is to take in IR from clover, and turn it into machine code for the target platform. This may be done through LLVM, Cranelift, or a similar library.