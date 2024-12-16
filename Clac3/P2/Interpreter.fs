module Clac3.P2.Interpreter

open Clac3.Expression
open Clac3.FunctionalExpression
open Clac3.Function

// TODO: currying, possible edge cases
// TODO: functions should be able to return functions

let rec handleNode (bindings: S2.BindingStore) (fnCall: S2.FunctionCall) =
    // is always some, or it would fail in normalization
    // TODO: then it shouldn't be an option
    match bindings[fnCall.ident].Value with
    | BValue v -> v
    | BuiltIn fn -> fnCall.args |> Array.map (evalExpr bindings) |> fn
    | Custom (argIdents, body) ->
        let args = fnCall.args |> Array.map (evalExpr bindings)
        // using Array.copy instead is not faster
        let mutable overwrittenBindings = []

        args |> Array.iteri (fun i arg ->
            overwrittenBindings <- (argIdents[i], bindings[argIdents[i]])::overwrittenBindings
            bindings[argIdents[i]] <- Some (BValue arg)
        )

        let result = evalExpr bindings body

        overwrittenBindings |> List.iter (fun (i, value) -> bindings[i] <- value)

        result
    
and evalExpr (bindings: S2.BindingStore) = function
    | FAtom a -> a
    | FArray a -> a |> Array.map (evalExpr bindings) |> FArray
    | FCall fnCall -> handleNode bindings fnCall
    | FBranch { cond = cond; trueB = trueB; falseB = falseB } ->
        match evalExpr bindings cond with
        | Bool true -> evalExpr bindings trueB
        | Bool false -> evalExpr bindings falseB
        | a -> failwithf "Expected bool, got %A" a