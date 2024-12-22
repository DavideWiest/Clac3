module Clac3.Type

open Clac3.Expression

// separate in and output types just for convenience
type FnSignature = {
    args: Type list
    returnType: Type
}

and Type =
    | TBool
    | TInteger
    | TFloat
    | TString
    | TVariable
    | TKeyword
    | TArray of Type

    | TLambda of FnSignature

    | TUnit

type TAExpression = {
    expr: Expression
    eType: Type
}
