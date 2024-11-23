module Clac3.Interpreter

open Clac3.Util
open Clac3.Domain
open Clac3.DomainUtil

// a faster search would construct a tree from the rules and search it

let rec getNextEvalChildI (children: Expression list) i =
    children
    |> List.skip i
    |> List.tryFindIndex (isDefined >> not)
    |> Option.map (fun index -> index + i)

let rec getNextEvalChildIExpr (expr: Expression) i =
    match expr with
    | Node children -> getNextEvalChildI children i
    | _ -> None

let isBreadthFirst = function
    | Node (Keyword "if"::_) -> true
    | _ -> false

let rec matchesGetter getter expr =
    match getter, expr with
    | TEvaluatedLeaf, Variable _
    | TEvaluatedLeaf, Node _ -> None
    | TEvaluatedLeaf, _ -> Some [expr]
    | TAny, _
    | TBool, Bool _
    | TInteger, Integer _
    | TFloat, Float _
    | TString, String _
    | TList, List _
    | TVariable, Variable _
    | TKeyword, Keyword _
    | TNode, Node _ -> Some [expr]
    | TNodeContaining tChildren, Node children -> 
        if tChildren.Length <> children.Length then None else 
        
        List.zip tChildren children 
        |> List.map (fun (p,e) -> matchesPattern p e)
        |> Option.combine
        |> Option.map List.concat
    | TNodeStartingWith tChildren, Node children -> 
        if tChildren.Length > children.Length then None else 
        
        List.zip tChildren children[..tChildren.Length-1] 
        |> List.map (fun (p,e) -> matchesPattern p e) 
        |> Option.combine 
        |> Option.map List.concat
        |> Option.map (fun matched -> matched @ children[tChildren.Length..])
    | _ -> None

and matchesPattern pattern expr = 
    printfn "matching %A with %A" pattern expr
    match pattern with 
    | Value v -> if v = expr then Some [] else None
    | Get getter -> matchesGetter getter expr
        
let rec eval rules childEvalI (expr: Expression) =
    // if the expression is a leaf but not a variable, it is already evaluated
    if isDefined expr then expr else

    printfn "evaluating %A" expr

    let nextChildEvalI = getNextEvalChildIExpr expr childEvalI
    // breadth first (ie pattern matching on current expr) search if necessary, otherwise depth first (ie evaluating children first) if there are any unevaluated children
    let maybeNewExpr = 
        if isBreadthFirst expr || nextChildEvalI = None 
        then 
            rules 
            |> List.tryPick (fun rule -> matchesPattern rule.pattern expr |> Option.tupleWith rule) 
            |> Option.map (fun (rule, args) -> rule.replacement args) 
        else None

    match maybeNewExpr with
    // evaluate the modified node
    // dont use Option.defaultValue, it will evaluate even if the option is Some - didnt know F# was so pythonic
    | Some newExpr -> eval rules 0 newExpr
    // no rule was applicable: find and evaluate the first unevaluated child
    // at some point no rule will be applicable, so all children that are supposed to be evaluated will be
    | None ->
        match expr with
        | Node children ->
            match nextChildEvalI with
            // all children have been evaluated
            | None -> expr
            | Some nextEvalChildI ->
                children[..nextEvalChildI-1] @ [eval rules 0 children[nextEvalChildI]] @ children[nextEvalChildI+1..] 
                |> Node
                |> eval rules (nextEvalChildI+1)
        | _ -> expr

let evalProgram p = p.freeExpressions |> List.map (eval p.rewriteRules 0)