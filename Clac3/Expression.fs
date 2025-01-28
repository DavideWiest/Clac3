module Clac3.Expression

// Expressions
type Atom = 
    | Bool of bool
    | Integer of int
    | Float of float
    | String of string

    | Variable of string
    | Keyword of string

// the unit type is not included because it's represented by an empty node
type Expression =
    | Atom of Atom
    | Array of Expression list
    | Node of Expression list