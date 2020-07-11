# Simple Interactive Interpreter

A super simple interactive interpreter. Derived from [this kata](https://www.codewars.com/kata/52ffcfa4aff455b3c2000750).

## REPL

Simply `cargo run`.

## Supports

1. Assignment
   ```
   > x = 3.5
   3.5
   > x
   3.5
   > x = 2
   2
   ```

2. Functions
   ```
   > fn avg x y => (x + y) / 2
   OK
   > avg 2 4
   3
   > x = 2
   2
   > y = 6
   6
   > avg x (avg x y)
   3
   ```

3. Rebuild the AST according to contexts
   ```
   > fn f x y => (x + y) / 2
   OK
   > fn g z => z + 1
   OK
   > f g 1 g 2
   2.5
   ```

   ```
   > fn f x y a b => (x + y) / 2
   OK
   > fn g => 1
   OK
   > f g 1 g 2
   1
   ```

4. Naive error handling

## To do and To fix

- [ ] Use `Clone` instead of `.to_string()`.
- [ ] Some issues about applying functions to context-sensitive variables.
