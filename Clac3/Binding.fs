module Clac3.Binding

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

module S2 =
    type BindingValue = BindingValue<int, S2.FExpression>
    type Binding = Binding<int, S2.FExpression>
    type BindingStore = BindingValue<int, S2.FExpression> option array

// first string for closure name, second for reference name
// a tree structure would be more appropriate
type CtxPath = string list
type ReferenceStore = Map<CtxPath, Map<string, Type>>

let rec accessReferenceStore (store: ReferenceStore) (path: CtxPath) (name: string) = 
    let nextHigherLevel = List.rev >> List.tail >> List.rev
    let handleNotFound () = 
        if path.Length = 0 then None else accessReferenceStore store (nextHigherLevel path) name

    match (store.TryFind path) with
    | Some ctx -> 
        match ctx.TryFind name with
        | Some t -> Some t
        | None -> handleNotFound ()
    | None -> handleNotFound ()