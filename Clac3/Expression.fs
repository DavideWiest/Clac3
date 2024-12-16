module Clac3.Expression

// Expressions
type Atom = 
    | Bool of bool
    | Integer of int
    | Float of float
    | String of string


    | Variable of string
    | Keyword of string

and Expression =
    | Atom of Atom
    | Array of Expression array
    | Node of Expression list