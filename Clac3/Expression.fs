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
    | List of Expression list // list items *should* have the same type
    | Node of Expression list