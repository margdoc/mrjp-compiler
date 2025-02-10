# Latte Compiler

This code was written as a university project. The task description, written in Polish, can be found [here](https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2022/latte.html). The specification of the Latte language for this task is available [here](https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2022/Latte/).


I chose Haskell for this project because I enjoy working with it, and it was recommended by the lecturer.
This version (uploaded to GitHub) does not meet the technical requirements for code structure.

## Score
This code scored **30.5/34 points**, the highest score among students in my year.

## Compilation
```make```

## Code Structure
- `Makefile`
- `src/` - source files
- `build/latc_x86_64` - compiled BNFC files for the grammar described in `src/Grammar.cf`
- `build/Grammar/` - compiled BNFC files for the grammar described in `src/Grammar.cf`
- `lib/runtime.o` - compiled built-in functions from `src/runtime.c`
- `tests/` - test cases for the compiler

## Grammar
The current conflict is a well-known issue with `if (...) if (...) ... else ...`.
The parser resolves this conflict by applying the shift operation, ensuring that `else` is associated with the nearest preceding `if`, which aligns with our intuition.

## Language Extensions
- Arrays
- Structures
- Objects
- Virtual methods
- Garbage collection (instead of a la Rust's memory management)
- LCSE/GCSE (Local/Common Subexpression Elimination)
- Constant propagation (enabled under the `-O2` compilation flag)


## Built-in Functions
Library and helper functions are included in `lib/runtime.o` (source code in `src/runtime.c`).
