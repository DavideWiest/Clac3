module Clac3.P2.Interpreter

open Clac3.FExpression
open Clac3.Function

// TODO: currying, possible edge cases
// TODO: functions should be able to return functions

let rec handleNode (bindings: S2.BindingStore) (ident, argsBefore) =
    // is always some, or it would fail in normalization
    // TODO: then it shouldn't be an option
    match bindings[ident].Value with
    | BValue v -> v
    | BFuncDef fn ->
        let args = argsBefore |> Array.map (evalExpr bindings)

        match fn.lambda with
        | BuiltIn fn -> fn args
        | Custom (argIdents, body) ->
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
    | FCall fnCall -> handleNode bindings fnCall
    | FBranch (cond,ifB,elseB) ->
        match evalExpr bindings cond with
        | FBool true -> evalExpr bindings ifB
        | FBool false -> evalExpr bindings elseB
        | a -> failwithf "Expected bool, got %A" a