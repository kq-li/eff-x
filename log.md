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
