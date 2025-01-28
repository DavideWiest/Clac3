module Clac3.P2.Interpreter

open Clac3.Expression
open Clac3.FunctionalExpression
open Clac3.Binding

// TODO: currying, possible edge cases
// TODO: functions should be able to return functions

let rec handleNode (bindings: S2.BindingStore) (fnCall: S2.Reference) : S2.FExpression =
    // is always some, or it would fail in normalization
    // TODO: then it shouldn't be an option
    match bindings[fnCall.ident].Value with
    | BValue v -> v
    | BuiltIn fn -> fnCall.args |> Array.map (evalFExpr bindings) |> fn
    | Custom customFn ->
        let args = fnCall.args |> Array.map (evalFExpr bindings)
        // using Array.copy instead is not faster
        let mutable overwrittenBindings = []

        args |> Array.iteri (fun i arg ->
            overwrittenBindings <- (customFn.argIdents[i], bindings[customFn.argIdents[i]])::overwrittenBindings
            bindings[customFn.argIdents[i]] <- Some (BValue arg)
        )

        let result = evalFExpr bindings customFn.body

        overwrittenBindings |> List.iter (fun (i, value) -> bindings[i] <- value)

        result
    
and evalFExpr (bindings: S2.BindingStore) = function
    | FUnit -> FUnit
    | FAtom a -> FAtom a
    | FArray a -> a |> Array.map (evalFExpr bindings) |> FArray
    | FRef fnCall -> handleNode bindings fnCall
    | FBranch branch ->
        match evalFExpr bindings branch.cond with
        | FAtom (FBool true) -> evalFExpr bindings branch.trueB
        | FAtom (FBool false) -> evalFExpr bindings branch.falseB
        | a -> failwithf "Expected bool, got %A" a