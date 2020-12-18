## 11/09
### 3 hours
- Implemented lexer and parser for lambda calculus + arithmetic expressions + sequencing
    - Needed special precedence declarations to resolve shift reduce
- Wrote basic lambda calculus interpreter using beta-reduction, alpha-conversion, and recursive evaluation
- Hooked up a REPL for simple testing

## 11/12
### 1 hour
- Implemented type annotations and a simple type checker

## 11/16
### 2 hours
- Implemented `With_effect` type that extends a type with a set of effects
- Added lexer and parser support for effects
- Note: `int!output` indicates an `int`-valued expression with an `output` side effect, like `print 5; 6`, but evaluating this expression changes the type to just `int`

## 12/13
### 2 hours
- Weighed pros and cons of eager vs. lazy evaluation/compilation
    - Lazy makes it possible for unevaluated expressions to have effect types
    - Eager means only deferred computations (i.e. thunks) can have effects
- Pivoted towards lower-level language (more like Bril)
    - Necessary to do any sort of meaningful reordering later
- Theorized that exprs can have effects, and commands just provide an order on exprs
    - Can be implemented with unops/binops or native funcs
    - Sequences of assignments of exprs can be reordered and optimized

## 12/15
### 4 hours
- Reimplemented interpreter and typechecker for new lower-level language

## 12/16
### 4 hours
- Pivoted back towards functional syntax
- Implemented higher-order functions, currying, and partial application

## 12/17
### 4 hours
- Implemented booleans
- Extended standard library (builtins)
    - Moved unary and binary ops to function land for more streamlined code
        - Had to manually implement currying for the library functions
    - Added load/store library functions to simulate memory
- Set up Turnt for regression tests
- Thought about advantages of the system
    - Precompute pure functions
    - Potentially compute in parallel

## 12/18
### 4 hours
- Reimplemented library to yet again fix currying
- Added input effect
- Wrote basic optimizer to precompute pure expressions
    - Set up script to compare statement counts of base and optimized programs
