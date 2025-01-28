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
    | TKeyword

    | TArray of Type option

    | TLambda of FnSignature

    | TUnit

// Type annotated expression
type TAExpressionValue = 
    | TAAtom of Atom
    // should all have the same type - which should be in the domain here
    // arrays being stored as list is ok for now as declared arrays aren't that big
    | TAArray of TAExpression list
    | TANode of TAExpression list

and TAExpression = {
    expr: TAExpressionValue
    eType: Type
}

let typesMatch a b = 
    match a, b with
    // special cases for empty array
    | TArray _, TArray None -> true
    | TArray None, TArray _ -> true
    | _ -> a = b