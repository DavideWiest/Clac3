module Clac3.P1.SubstitutionInterpreter

open Clac3.Function
open Clac3.P1.Expression
open Clac3.P1.TypeAnnotatedExpression

let rec handleListLikeType wrapperFn tryReplace defaultFn children =
    let children' = children |> List.map (evalExpr tryReplace defaultFn)
    children' |> wrapperFn |> tryReplace |> Option.map (evalExpr tryReplace defaultFn) |> Option.defaultValue (defaultFn (wrapperFn children'))
    
and evalExpr (tryReplace: Expression -> Expression option) (defaultFn: Expression -> Expression) = function
    | Atom a -> tryReplace (Atom a) |> Option.defaultValue (defaultFn (Atom a))
    | List children -> handleListLikeType List tryReplace defaultFn children
    | Node children -> handleListLikeType Node tryReplace defaultFn children

let rec toTAExprInner (bindingStore: S1.BindingStore) = function
    | Atom a -> TAAtom (TBool, a)
    | List children -> TAList (TList TBool, children |> List.map (toTAExprInner bindingStore))
    | Node (children) -> 
        match children with
        // might support this later, but it wouldn't be useful since Clac is lazily evaluated
        | [] -> failwith "Empty node"
        | head::tail -> 
            match head with
            | Atom (Keyword fnName) -> TANode (TBool, (fnName, tail |> List.map (toTAExprInner bindingStore)))
            | Node children -> toTAExprInner bindingStore (Node (children@tail)) // flatten the node
            | _ -> failwith "Evaluated node in phase 1 must be convertible to functional form"
        
let toTAExpr functionStore tryReplace defaultFn expr = 
    expr
    |> evalExpr tryReplace defaultFn
    |> toTAExprInner functionStore