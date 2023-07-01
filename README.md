# Documentation
This language is a simple interpreted functional programming language similar to Haskell / OCaml.

# Build
To build the project, simply run ```ghc lang```

# Running Code
Any file can be intepreted by running ```./lang [FILENAME]```

# REPL
The language also contains a REPL which can be accessed by running ```./lang``` with no arguments. From here, expression can be interpreted by entering them into the prompt.
```
> + 1 2
3
```
Functions can be defined and referenced:
```
> func (x: float) -> + 1 x
> func 12
13
```
Files can be loaded into the REPL by using the ```load``` command (including the .z extension is optional):
```
> load foo
Loaded from foo.z...
> abs 12 13
1
```

# Tutorial
## Run Function
The run function is the foundation of any script and the code that will be executed when a script is ran. 
```
run -> + 1 2
```

## Prefix Notation
One thing to note is that addition is called using prefix notation. This is the case for all binary operators and functions. The following lines of code are all valid binary operators, and the logic extends to all operators of the same class (arithmetic, logic, etc.)
```
+ 1 2
> 1 2
== 1 2
|| true false
```
Binary operators can be nested using proper placement of parenthesis:
```
+ (- 3 2) 1
```
If statements can also be used to change control flow based on the conditions:
```
if 1 > 2 then 3 else 4
````

## Functions
Just using the run function limits the user to basic binary operations. Functions enable the user to use pattern matching and recursion to create more sophisticated operations.\
Functions are created in a way similar to the run function but with the addition parenthesis, variable names, and type definitions. For instance, the following function can be used to add two numbers together:
```
add (x: float, y: float) -> + x y
```
This is still limited since functions effectively become abbreviated expressions. Pattern matching allows for control flow in functions. Take the following function:
```
func (x: float)
    | 0 -> 0
    | 1 -> -1
    | x -> * x 12
```
If the value being passed to the function is zero, the function returns zero. If the value is one, the function returns -1. Otherwise the function will return 12 times whatever was passed through. A variable pattern is a wildcard and will always match the given expression.\

Functions can also call themselves, which allows for recursion. For instance, the following function can be used to calculate the nth fibonacci number:
```
fib (x: float)
    | 0 -> 0
    | 1 -> 1
    | x -> + (fib (- x 1)) (fib (- x 2))
```

# WARNING: Error Checking
There is error checking in the language, but it is currently minimal