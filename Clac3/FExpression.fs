module Clac3.FExpression

type FAtom =
    | FBool of bool
    | FInteger of int
    | FFloat of float
    | FString of string
    | FArray of FAtom array

type FunctionCall<'a> = 'a * (FExpression<'a> array)

and FExpression<'a> =
    | FAtom of FAtom
    | FCall of FunctionCall<'a>
    | FBranch of FExpression<'a> * FExpression<'a> * FExpression<'a>
   
module S1 =
    type FunctionCall = FunctionCall<string>
    type FExpression = FExpression<string>
    
module S2 =
    type FunctionCall = FunctionCall<int>
    type FExpression = FExpression<int>