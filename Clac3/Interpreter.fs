module Clac3.Interpreter

open Clac3.Domain
open Clac3.Representation

// a faster search would construct a tree from the rules and search it

let rec getNextEvalChildI (children: Expression list) i =
    if i >= children.Length then None else
    if exprIsDefinedLeaf children[i] then getNextEvalChildI children (i+1) else Some i

let rec getNextEvalChildIExpr (expr: Expression) i =
    match expr with
    | Node children -> getNextEvalChildI children i
    | _ -> None

let isBreadthFirst (expr: Expression) = 
    match expr with
    | Node (Leaf(Keyword "if")::rest) -> true
    | _ -> false
        
let rec eval rules childEvalI (expr: Expression) =
    // if the expression is a leaf but not a variable, it is already evaluated
    if exprIsDefinedLeaf expr then expr else

    let nextChildEvalI = getNextEvalChildIExpr expr childEvalI    
    // breadth first (ie pattern matching on current expr) search if necessary, otherwise depth first (ie evaluating children first) if there are any unevaluated children
    let maybeNewExpr = if isBreadthFirst expr || nextChildEvalI = None then List.tryPick (fun rule -> rule.pattern expr) rules else None

    match maybeNewExpr with
    // evaluate the modified node
    // dont use Option.defaultValue, it will evaluate even if the option is Some - didnt know F# was so pythonic
    | Some newExpr -> eval rules 0 newExpr
    // no rule was applicable: find and evaluate the first unevaluated child
    // at some point no rule will be applicable, so all children that are supposed to be evaluated will be
    | None ->
        match expr with
        | Leaf _ -> expr
        | Node children ->
            match nextChildEvalI with
            // all children have been evaluated
            | None -> expr
            | Some nextEvalChildI ->
                children[..nextEvalChildI-1] @ [eval rules 0 children[nextEvalChildI]] @ children[nextEvalChildI+1..] 
                |> Node
                |> eval rules (nextEvalChildI+1)
    

let evalProgram p = p.freeExpressions |> List.map (eval p.rewriteRules 0)