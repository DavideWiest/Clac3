module Clac3.Interpreter

open Clac3.Util
open Clac3.Domain
open Clac3.DomainUtil


module DecisionTree =
    type PatternDecisionTree =
        | LeafEdge of Pattern * (Expression list -> Expression) // any leaf pattern
        | NodeEdge of NodeDecisionTree list // a node/recursive pattern

    // the first level of a faster decision tree would decide based on the length of the node
    // PDT for the head, NDT for the tail
    and NodeDecisionTree =
        | NDTLeaf of PatternDecisionTree
        | NDTNode of PatternDecisionTree * (NodeDecisionTree list)

    type TreeResult = ((Expression list -> Expression) * Expression list) option

    let rec printPatternDecisionTree (indent: int) (tree: PatternDecisionTree) =
        let indentStr = String.replicate indent ".   "
        match tree with
        | LeafEdge(pattern, func) ->
            printfn "%sPattern leaf: \n%s.   %A => %A" indentStr indentStr pattern func
        | NodeEdge(nodeTrees) ->
            printfn "%sPattern node:" indentStr
            nodeTrees |> List.iter (printNodeDecisionTree (indent + 1))

    and printNodeDecisionTree (indent: int) (nodeTree: NodeDecisionTree) =
        let indentStr = String.replicate indent ".   "
        match nodeTree with
        | NDTLeaf patternTree ->
            printfn "%sNode leaf:" indentStr
            printPatternDecisionTree (indent + 1) patternTree
        | NDTNode(patternTree, children) ->
            printfn "%sNode node:" indentStr
            printfn "%s?" indentStr
            printPatternDecisionTree (indent + 1) patternTree
            printfn "%s=>" indentStr
            children |> List.iter (printNodeDecisionTree (indent + 1))

    let getPDTSearchPriority = function
        | LeafEdge(p, _) ->
            match p with
            | PBoolValue _ -> 0
            | PIntegerValue _ -> 1
            | PFloatValue _ -> 2
            | PStringValue _ -> 3
            | PVariableValue _ -> 4
            | PKeywordValue _ -> 5
            | PListValue _ -> 6

            | PNodeContaining children -> failwithf "Expected a leaf pattern, got %A" children
            //| PNodeStartingWith _ -> failwithf "Expected a leaf pattern, got %A" children

            | PBool -> 209
            | PInteger -> 210
            | PFloat -> 211
            | PString -> 212
            | PVariable -> 213
            | PKeyword -> 214
            | PList -> 215
            | PNode -> 216

            | PAnyEvaluatdLeaf -> 316
            | PAny -> 317
        | NodeEdge _ -> 1000

    let getNDTSearchPriority = function
        | NDTLeaf _ -> 0
        | NDTNode _ -> 1

    type NodeRewriteRule = {
        patternList: Pattern list
        replacer: Expression list -> Expression
    }

    let tryGetChildren = function
        | PListValue children
        | PNodeContaining children -> Some children
        | _ -> None

    let toNodeRewriteRule replacer children = { patternList = children; replacer = replacer }

    type Matcher(rules: RewriteRule list) =
        member private this.getNDT (rules: NodeRewriteRule list) : NodeDecisionTree list =
            if rules.Length = 0 then failwith "Got no rules for building a NDT"

            let emptyRules = rules |> List.filter (fun rule -> rule.patternList.Length = 0)
            if emptyRules.Length > 0 then failwithf "Got empty rules: %A" emptyRules

            let ruleGroups = rules |> List.groupBy (fun rule -> rule.patternList.Head)

            let leaves: NodeDecisionTree list = ruleGroups |> List.choose (fun (head, ruleGroup) ->
                let leafRules = ruleGroup |> List.filter (fun rule -> rule.patternList.Length = 1)
                if leafRules.Length = 0 then None else
                // head must be different, but more than one rule matches
                if leafRules.Length > 1 then failwithf "Got multiple leaf rules for the same pattern: %A" leafRules 

                let pdt = this.getSinglePDT { pattern = head; replacer = leafRules[0].replacer }
                (pdt) |> NDTLeaf |> Some
            ) 
                
            let nodes: NodeDecisionTree list = ruleGroups |> List.choose (fun (head, rules) ->
                let nodeRules = 
                    rules 
                    |> List.filter (fun rule -> rule.patternList.Length > 1)
                    |> List.map (fun rule -> { rule with patternList = rule.patternList.Tail })
                if nodeRules.Length = 0 then None else

                let pdt = this.getSinglePDT { pattern = head; replacer = fun _ -> failwith "Call to replacer for a pattern that is not fully matched" }
                let ndt = this.getNDT nodeRules
                (pdt, ndt) |> NDTNode |> Some
            )
            
            (nodes@leaves) |> List.sortBy getNDTSearchPriority

        member private this.getSinglePDT (rule: RewriteRule) =
            match tryGetChildren rule.pattern with
            | Some children -> 
                let ndt = this.getNDT [{ patternList = children; replacer = rule.replacer }]
                NodeEdge ndt
            | None -> LeafEdge(rule.pattern, rule.replacer)

        member private this.getPDT (rules: RewriteRule list) : PatternDecisionTree list =
            if rules.Length = 0 then failwith "Got no rules for building a PDT"

            let leafEdges = 
                rules 
                |> List.filter (fun rule -> tryGetChildren rule.pattern = None) 
                |> List.map (fun rule -> rule.pattern, rule.replacer) 
                |> List.map LeafEdge

            let nodeEdges = 
                rules 
                |> List.choose (fun rule -> tryGetChildren rule.pattern |> Option.map (toNodeRewriteRule rule.replacer)) 
                |> this.getNDT
                |> NodeEdge

            nodeEdges::leafEdges |> List.sortBy getPDTSearchPriority

        member private this.tree = this.getPDT rules
        member this.printTree = this.tree |> List.iter (printPatternDecisionTree 0)

        // make this fail in some cases as the tree is being walked now
        member private this.matchesLeafPattern pattern expr =
            match pattern, expr with
            | PAnyEvaluatdLeaf, Variable _
            | PAnyEvaluatdLeaf, Node _ -> None

            | PAny, _
            | PAnyEvaluatdLeaf, _
            | PBool, Bool _
            | PInteger, Integer _
            | PFloat, Float _
            | PString, String _
            | PList, List _
            | PVariable, Variable _
            | PKeyword, Keyword _
            | PNode, Node _ -> Some [expr]

            | PBoolValue b1, Bool b2 when b1 = b2 -> Some []
            | PIntegerValue i1, Integer i2 when i1 = i2 -> Some []
            | PFloatValue f1, Float f2 when f1 = f2 -> Some []
            | PStringValue s1, String s2 when s1 = s2 -> Some []
            // should probably be changed to work like node
            | PListValue l1, List l2 -> 
                if l1.Length <> l2.Length then None else

                List.zip l1 l2 
                |> List.map (fun (p,e) -> this.matchesLeafPattern p e)
                |> Option.combine
                |> Option.map List.concat

            | PVariableValue v1, Variable v2 when v1 = v2 -> Some []
            | PKeywordValue k1, Keyword k2 when k1 = k2 -> Some []

            | PNodeContaining children, _ -> failwithf "Expected leaf pattern, got PNodeContaining %A" children
            //| PNodeStartingWith tChildren, Node children -> failwithf "Expected leaf pattern, got PNodeStartingWith %A" children

            | _ -> None

        member this.walkPDT tree argsAcc (expr: Expression) = 
            printfn "PATTERN DT\n    args %A\n    expr  %A" argsAcc expr
            match tree, expr with
            | NodeEdge patternChildren, Node children -> 
                List.fold (fun (maybeResult: TreeResult) childTree -> 
                    if maybeResult.IsSome 
                    then maybeResult
                    else 
                        printfn "NODE CHILD" 
                        printNodeDecisionTree 1 childTree
                        this.walkNDT childTree argsAcc children
                ) None patternChildren
            | LeafEdge(pattern, replacer), expr -> 
                printfn "PATTERN LEAF"
                printPatternDecisionTree 1 tree
                this.matchesLeafPattern pattern expr |> Option.map (fun args -> replacer, args)
            | _ -> None

        member this.walkNDT tree argsAcc (exprs: Expression list) =
            printfn "NODE DT\n    args %A\n    exprs %A" argsAcc exprs
            match tree with
            | NDTLeaf(patternTree) -> 
                if exprs.Length <> 1 then None else
                printfn "NODE LEAF"
                printPatternDecisionTree 1 patternTree
                this.walkPDT patternTree argsAcc exprs[0] |> Option.map (fun treeResult -> treeResult |> fst, argsAcc@(treeResult |> snd))
            | NDTNode(patternTree, children) -> 
                match this.walkPDT patternTree argsAcc exprs[0] with
                | Some (_, args) -> 
                    List.fold (fun maybeResult childTree -> 
                        if maybeResult.IsSome 
                        then maybeResult 
                        else 
                            printfn "NODE CHILD" 
                            printNodeDecisionTree 1 childTree
                            this.walkNDT childTree args exprs[1..]
                    ) None children
                | None -> None

        member this.tryGetReplacementFor expr = 
            printfn "REPLACING %A" expr
            List.fold (fun (maybeResult: TreeResult) childTree -> if maybeResult.IsSome then maybeResult else this.walkPDT childTree [] expr) None this.tree
            |> Option.map (fun (replacer, args) -> replacer args)
      
module Evaluator =
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

    let rec eval (tree: DecisionTree.Matcher) childEvalI (expr: Expression) =
        // if the expression is a leaf but not a variable, it is already evaluated
        if isDefined expr then expr else

        let nextChildEvalI = getNextEvalChildIExpr expr childEvalI
        // breadth first (ie pattern matching on current expr) search if necessary, otherwise depth first (ie evaluating children first) if there are any unevaluated children
        let maybeNewExpr = if isBreadthFirst expr || nextChildEvalI = None then tree.tryGetReplacementFor expr else None

        match maybeNewExpr with
        // evaluate the modified node
        // dont use Option.defaultValue, it will evaluate even if the option is Some - didnt know F# was so pythonic
        | Some newExpr -> eval tree 0 newExpr
        // no rule was applicable: find and evaluate the first unevaluated child
        // at some point no rule will be applicable, so all children that are supposed to be evaluated will be
        | None ->
            match expr with
            | Node children ->
                match nextChildEvalI with
                // all children have been evaluated
                | None -> expr
                | Some nextEvalChildI ->
                    children[..nextEvalChildI-1] @ [eval tree 0 children[nextEvalChildI]] @ children[nextEvalChildI+1..] 
                    |> Node
                    |> eval tree (nextEvalChildI+1)
            | _ -> expr

let evalProgram p = 
    let decisioinTree = DecisionTree.Matcher(p.rewriteRules)
    decisioinTree.printTree
    p.freeExpressions |> List.map (Evaluator.eval decisioinTree 0)