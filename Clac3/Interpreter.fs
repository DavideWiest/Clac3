module Clac3.Interpreter

open Clac3.Domain
open Clac3.DecisionTree
    
let private isDefined = function
    | Atom a -> 
        match a with
        | Variable _ -> false
        | _ -> true
    | Node _ -> false 
    | List _ -> false

let rec private getNextEvalChildI (children: Expression list) i =
    children
    |> List.skip i
    |> List.tryFindIndex (isDefined >> not)
    |> Option.map (fun index -> index + i)

let rec private getNextEvalChildIExpr (expr: Expression) i =
    match expr with
    | Node children -> getNextEvalChildI children i
    | _ -> None

let private isBreadthFirst = function
    | Node (Atom (Keyword "if")::_) -> true
    | _ -> false

let rec evalExpr (tree: DecisionTree.Walker) childEvalI (expr: Expression) =
    // if the expression is a leaf but not a variable, it is already evaluated
    if isDefined expr then expr else

    let nextChildEvalI = getNextEvalChildIExpr expr childEvalI
    // breadth first (ie pattern matching on current expr) search if necessary, 
    // otherwise depth first (ie evaluating children first) if there are any unevaluated children
    let maybeNewExpr = if isBreadthFirst expr || nextChildEvalI = None then tree.tryReplace expr else None

    match maybeNewExpr with
    // evaluate the modified node
    // dont use Option.defaultValue, it will evaluate even if the option is Some - didnt know F# was so pythonic
    | Some newExpr -> evalExpr tree 0 newExpr
    // no rule was applicable: find and evaluate the first unevaluated child
    // at some point no rule will be applicable, so all children that are supposed to be evaluated will be
    | None ->
        match expr with
        | Node children ->
            match nextChildEvalI with
            // all children have been evaluated
            | None -> expr
            | Some nextEvalChildI ->
                children[..nextEvalChildI-1] @ [evalExpr tree 0 children[nextEvalChildI]] @ children[nextEvalChildI+1..] 
                |> Node
                |> evalExpr tree (nextEvalChildI+1)
        | _ -> expr