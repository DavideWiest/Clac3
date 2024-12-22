module Clac3.FunctionalExpression

type FAtom = 
    | FBool of bool
    | FInteger of int
    | FFloat of float
    | FString of string

type Reference<'a> = {
    ident: 'a
    args: FExpression<'a> array
}

// todo: branch and function bindings should contain a list of extra bindings

and Branch<'a> = {
    cond: FExpression<'a>
    trueB: FExpression<'a>
    falseB: FExpression<'a>
}

and FExpression<'a> =
    | FUnit
    | FAtom of FAtom
    | FArray of FExpression<'a> array
    | FRef of Reference<'a>
    | FBranch of Branch<'a>
   
module S1 =
    type Reference = Reference<string>
    type FExpression = FExpression<string>
    
module S2 =
    type Reference = Reference<int>
    type FExpression = FExpression<int>