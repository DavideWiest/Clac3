module Clac3.P1.SubstitutionInterpreter

open Clac3.P1.Expression

let rec handleListLikeType wrapperFn tryReplace defaultFn children =
    let children' = children |> List.map (evalExpr tryReplace defaultFn)
    children' |> wrapperFn |> tryReplace |> Option.map (evalExpr tryReplace defaultFn) |> Option.defaultValue (defaultFn (wrapperFn children'))
    
and evalExpr (tryReplace: Expression -> Expression option) (defaultFn: Expression -> Expression) (expr: Expression) =
    match expr with
    | Atom _ -> tryReplace expr |> Option.defaultValue (defaultFn expr)
    | List children -> handleListLikeType List tryReplace defaultFn children
    | Node children -> handleListLikeType Node tryReplace defaultFn children