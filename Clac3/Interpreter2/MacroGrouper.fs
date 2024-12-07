module Clac3.Interpreter2.MacroGrouper

open Clac3.Domain

let rec applyToAllAtoms f (p: Pattern) =
    let rec applyToAllAtoms' = function
        | PDefined (Value a) -> PDefined (Value (f a))
        | PList (Value ps) -> PList (Value (List.map (applyToAllAtoms f) ps))
        | PNode (Value ps) -> PNode (Value (List.map (applyToAllAtoms f) ps))
        | expr -> expr 

    match p with
    | Value a -> Value (applyToAllAtoms' a)
    | Any s -> Any s

let aggregateKeywords node =
    let kws, noKws = List.partition (function Value (PDefined (Value (PKeyword _)))  -> true | _ -> false) node
    let kwsDefined = kws |> List.map (function Value (PDefined (Value (PKeyword (Value k)))) -> k | _ -> failwith "Any-Keyword is not allowed")
    let kwsCombined = kwsDefined |> String.concat "" |> Value |> PKeyword |> Value |> PDefined |> Value

    kwsCombined::noKws

let rec applyToAllNodes f (p: Pattern) =
    let rec applyToAllNodes' = function
        | PDefined a -> PDefined (a)
        | PList (Value ps) -> PList (Value (List.map (applyToAllNodes f) ps))
        | PNode (Value ps) -> PNode (Value (List.map (applyToAllNodes f) ps |> f))
        | expr -> expr
    
    match p with
    | Value a -> Value (applyToAllNodes' a)
    | Any s -> Any s

let groupByPattern (macros: Macro list) =
    macros
    |> List.map (fun m -> )
    |> List.map 
    // build a defined-value-list -> replacer relation
    // apply the aggregateKeywords function to only the pattern
    |> List.iter (fun (p, ms) -> printfn "-> %A: \n%s" p (ms |> List.map string |> List.map (fun s -> ".   "  + s) |> String.concat "\n"))



