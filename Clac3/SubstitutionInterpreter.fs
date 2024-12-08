module Clac3.SubstitutionInterpreter

open Clac3.Domain

let rec handleListLikeType wrapperFn tryReplace children =
    children |> List.map (evalExpr tryReplace) |> wrapperFn |> tryReplace |> Option.map (evalExpr tryReplace) |> Option.defaultValue (wrapperFn children)
    
and evalExpr (tryReplace: Expression -> Expression option) (expr: Expression) =
    match expr with
    | Atom _ -> tryReplace expr |> Option.defaultValue expr
    | List children -> handleListLikeType List tryReplace children
    | Node children -> handleListLikeType Node tryReplace children