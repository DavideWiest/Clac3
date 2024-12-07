# Clac3

### Manifest
##### Atomic data
```
Comment: // comment

Bool: true, false
Integer: 1,2,3
Float: 1., 1.1, 2.2
Char: 'c'
String: "hello world"
```

##### Data structures
```
Arrays: [1,2,3]
Tuple: ("pi", 3.141)
```

##### Exception handling
```
Errors: Error "The operation failed.", ArgumentError "The growth rate must be positive"
Catching errors: try ... with OneError -> ...
```

##### Functions
```
Lambda: n:int -> n * n
Function shorthand: square:int n:int -> n * n
Argument checks: squareNeg:int n:(int, <0) -> n * n * -1 // type always required with this syntax. signature: argT -> bool
Multi-line form: 
    compound:float
        startingCapital:float
        growthRate:float/time
        duration:time
    ->
        startingCapital * (1+growthRate) ^ duration
Currying: supported
Calling function: compound 10 5%/y 30y
```

##### Signatures
```
Function (compound): float -> float/time -> time -> float // arrows to separate args, to allow for types like "int list option"
Any-type: *
Tuple: (string, float)
```

##### Functions: Nice to have 
```
Arguments shortform:
    compound:float
        startingCapital(s):float
        growthRate(g):float/time
        duration(t):time
    ->
        s * (1+g) ^ t
Annotated (partial) application
    compound growthRate=5%/y duration=30y // => float -> float
```

##### Variables
```
Variables: pi, growth
pi:float -> 3.1415926
Nice to have - Mutability: mut count:int -> 0
```

##### Records and classes
```
Definition:
    Coords:rec 
        x:int 
        y:int 
        z:int
    // => int -> int -> int -> Coords
Instantiation:
    coords:Coords -> Coords 1 2 3 // the type and instantiator-function have the same name
Overriding:
    coords with x=5
Access:
    coords.x
Class:
    Car:obj
        coords:Coords
        drive:nil
            distance(d):int
        ->
            this with coords = (this.coords with x=x+d) // nice to have: variable scope for with-updates
            // nice to have: mutability (unrealistic)
            // methods are also nice-to-have for now
        // => Car -> int -> Car
```

##### Control structures
```
Branch statements:
    if _ then _ else _ // => bool -> * -> * -> *
    _ ? _ : _
Loops: methods of arrays, sequences, lists (especially sequences) (really no while loop?)
Nice to have: match statements + variable extraction from objects (e.g. Some(value) where value is a variable)
```

##### Utilities
```
Unit: 1.5m, 3cm, 1m/s, 1m/s^2, 2N
Percentage function: 5%
Scientific notation: 5e-9

Nice to have - automatic conversions:
    Bool -> Int // at least for multiplication - * overload suffices
    Int -> Float
    Char -> String
    Units: to metric units
```

##### Modularization
```
Module syntax:
    module ...
    open ...
Modules within files: supported
```

##### Capabilities
- simple DSLs using Macros
- as fast as python, roughly

### Observations
- highly functional implementation
    - instantiation of records and classes as functions
    - inner values of class/record as array of different values
    - attributes access array at different indices
    - methods as functions with the class appended to the args
- function overloading
    - function identified by keywords and types of args (a compromise between function and pattern)
        - keyword may be aggregated and moved to the front - duplicates that would occur this way are caused by unclear syntax and shouldn't exist anyway
            - e.g. factorial 5 <-> 5 factorial
    - different macros with the same signature 
- macros 
- so syntactical differentiation between keyword/syntax and variables
    - probably not that hard/costly
- currying supported via pattern matching when applicable (bad strategy?)