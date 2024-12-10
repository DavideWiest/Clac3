module Clac3.P1.TypeAnnotatedExpression

open Clac3.P1.Expression

type Type =
    | TBool
    | TInteger
    | TFloat
    | TString
    | TVariable
    | TKeyword
    | TList of Type

type TypeAnnotatedExpression =
    | TAAtom of Atom
    // list is a recursive type, but won't evaluate to some other type 
    // the annotation is only for node, as its the only option that can evaluate to a different type
    // however: maybe a list should have a type too, for type checking
    | TAList of TypeAnnotatedExpression list 
    | TANode of TypeAnnotatedExpression list * Type