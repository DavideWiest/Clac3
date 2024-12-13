module Clac3.P2.Function

open Clac3.P2.FExpression

type FunctionBody<'a, 'b> = 
    | BuiltIn of (FAtom array -> FAtom) 
    | Custom of ('a array) * 'b

type FunctionDefinition<'a, 'b> = {
    ident: 'a
    lambda: FunctionBody<'a, 'b>
}

// TODO: different stores for these 2??
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
 
