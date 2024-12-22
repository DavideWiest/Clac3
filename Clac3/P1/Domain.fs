module Clac3.P1.Domain

open Clac3.Expression
open Clac3.Type

// this should be closures, and a lot more flexible. but suffices as first version
// also, the returntype can be inferred

type RawCustomFn = {
    argIdents: string array
    body: Expression
}

type RawBindingValue = 
    | RValue of Expression
    | RCustom of RawCustomFn

type RawBinding = {
    ident: string
    signature: FnSignature
    body: RawBindingValue
}