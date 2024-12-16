module Clac3.FunctionalExpression

open Clac3.Expression

type FunctionCall<'a> = {
    ident: 'a
    args: FExpression<'a> array
}

and Branch<'a> = {
    cond: FExpression<'a>
    trueB: FExpression<'a>
    falseB: FExpression<'a>
}

and FExpression<'a> =
    | FAtom of Atom
    | FArray of FExpression<'a> array
    | FCall of FunctionCall<'a>
    | FBranch of Branch<'a>
   
module S1 =
    type FunctionCall = FunctionCall<string>
    type FExpression = FExpression<string>
    
module S2 =
    type FunctionCall = FunctionCall<int>
    type FExpression = FExpression<int>