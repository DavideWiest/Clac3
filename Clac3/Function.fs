module Clac3.Function

open Clac3.FExpression
open Clac3.TypeAnnotatedExpression

type FunctionBody<'a, 'b> = 
    | BuiltIn of (FAtom array -> FAtom) 
    | Custom of ('a array) * 'b

type FunctionDefinition<'a, 'b> = {
    ident: 'a
    signature: Type list * Type
    lambda: FunctionBody<'a, 'b>
}

type Binding<'a, 'b> = 
    | BValue of FAtom
    | BFuncDef of FunctionDefinition<'a, 'b>

module S1 =
    type FunctionBody = FunctionBody<string, S1.FExpression>
    type FunctionDefinition = FunctionDefinition<string, S1.FExpression>
    type Binding = Binding<string, S1.FExpression>
    type BindingStore = Map<string, Binding<string, S1.FExpression>>

module S2 =
    type FunctionBody = FunctionBody<int, S2.FExpression>
    type FunctionDefinition = FunctionDefinition<int, S2.FExpression>
    type Binding = Binding<int, S2.FExpression>
    type BindingStore = Binding<int, S2.FExpression> option array
 
