module Clac3.Function

open Clac3.FunctionalExpression
open Clac3.Type

type CustomFn<'a, 'b> = {
    argIdents: 'a array
    body: 'b
}

type BindingValue<'a, 'b> = 
    | BValue of 'b
    | BuiltIn of (S2.FExpression array -> S2.FExpression) // can't be atom because array (or any recursive types) can't be atoms
    | Custom of CustomFn<'a, 'b>

type Binding<'a, 'b> = {
    ident: 'a
    signature: FnSignature
    binding: BindingValue<'a, 'b>
}

type BuiltInBinding = BindingValue<string, S2.FExpression>

module S1 =
    type BindingValue = BindingValue<string, S1.FExpression>
    type Binding = Binding<string, S1.FExpression>
    type BindingStore = Map<string, Binding<string, S1.FExpression>>

    type ReferenceStore = Map<string, Type>

module S2 =
    type BindingValue = BindingValue<int, S2.FExpression>
    type Binding = Binding<int, S2.FExpression>
    type BindingStore = BindingValue<int, S2.FExpression> option array

