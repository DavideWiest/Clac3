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

    | TFunc of Type list * Type // separate in and output types just for convenience

type TAFnCall = string * (TypeAnnotatedExpression list)

and TypeAnnotatedExpression =
    | TAAtom of Type * Atom
    | TAList of Type * TypeAnnotatedExpression list 
    | TANode of Type * TAFnCall
