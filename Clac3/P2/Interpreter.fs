module Clac3.P2.Interpreter

open Clac3.P2.FExpression
open Clac3.P2.Function

// TODO: currying, possible edge cases
// TODO: functions should return the Binding type to allow for currying

// TODO: pass by reference + re-assign overwritten variables, storing them in a separate array

let rec handleNode bindingRelation (bindings: S2.BindingStore) (ident, argsBefore) =
    // is always some, or it would fail in normalization
    match bindings[ident].Value with
    | BValue v -> v
    | BFuncDef fn ->
        let args = argsBefore |> Array.map (evalExpr bindingRelation bindings)

        match fn.lambda with
        | BuiltIn fn -> fn args
        | Custom (argIdents, body) ->
            let mutable overwrittenBindings = []
            for i in 0..args.Length-1 do
                overwrittenBindings <- (argIdents[i], bindings[argIdents[i]])::overwrittenBindings
                bindings[argIdents[i]] <- Some (BValue args[i])

            let result = evalExpr bindingRelation bindings body

            for (i, value) in overwrittenBindings do 
                bindings[i] <- value

            result
    
and evalExpr bindingRelation (bindings: S2.BindingStore) (expr: S2.FExpression) =
    match expr with
    | FAtom a -> a
    | FCall fnCall -> handleNode bindingRelation bindings fnCall
    | FBranch (cond,ifB,elseB) ->
        match evalExpr bindingRelation bindings cond with
        | FBool true -> evalExpr bindingRelation bindings ifB
        | FBool false -> evalExpr bindingRelation bindings elseB
        | a -> failwithf "Expected bool, got %A" a