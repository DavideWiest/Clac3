module Clac3.Interpreter

open Clac3.Domain
open Clac3.DomainUtil

module DecisionTree =
    type PatternDecisionTree =
        | LeafEdge of LeafPattern * (Expression list -> Expression)
        | NodeEdge of NodeDecisionTree list // a node/recursive pattern
        | AnyEdge of (Expression list -> Expression)

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
        | AnyEdge func ->
            printfn "%sPattern any \n.    %s=> %A" indentStr indentStr func

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
        | NodeEdge _ -> -1
        | LeafEdge(p, _) ->
            match p with
            | PBoolValue _ -> 0
            | PIntegerValue _ -> 1
            | PFloatValue _ -> 2
            | PStringValue _ -> 3
            | PVariableValue _ -> 4
            | PKeywordValue _ -> 5

            | PBool -> 209
            | PInteger -> 210
            | PFloat -> 211
            | PString -> 212
            | PVariable -> 213
            | PKeyword -> 214
            | PList -> 215
            | PNode -> 216
        | AnyEdge _ -> 2000

    type NodeRewriteRule = {
        patternList: Pattern list
        replacer: Expression list -> Expression
    }

    let tryGetChildren = function
        | PListValue children
        | PNodeContaining children -> Some children
        | _ -> None

    let toNodeRewriteRule replacer children = { patternList = children; replacer = replacer }

    let isAnyPattern rule =
        match rule.pattern with
        | PAny -> true
        | _ -> false

    let isLeafPattern rule =
        match rule.pattern with
        | PLeaf _ -> true
        | _ -> false

    let getLeafPattern = function
        | PLeaf p -> p
        | _ -> failwith "Expected leaf pattern"

    let matchesLeafPattern pattern expr =
        //printfn "MATCHING %A%s WITH %A" expr (String.replicate (30-expr.ToString().Length) " ") pattern
        // nested match is more efficient
        // including this in the tree would be more efficient and relatively trivial
        match expr with
        | Node n ->
            match pattern with
            | PNode -> Some n
            | _ -> None
        | Bool b ->
            match pattern with
            | PBool -> Some [Bool b]
            | PBoolValue b2 when b = b2 -> Some []
            | _ -> None
        | Integer i ->
            match pattern with
            | PInteger -> Some [Integer i]
            | PIntegerValue i2 when i = i2 -> Some []
            | _ -> None
        | Float f ->
            match pattern with
            | PFloat -> Some [Float f]
            | PFloatValue f2 when f = f2 -> Some []
            | _ -> None
        | String s ->
            match pattern with
            | PString -> Some [String s]
            | PStringValue s2 when s = s2 -> Some []
            | _ -> None
        | List l ->
            match pattern with
            | PList -> Some l
            | _ -> None
        | Variable v ->
            match pattern with
            | PVariable -> Some [Variable v]
            | PVariableValue v2 when v = v2 -> Some []
            | _ -> None
        | Keyword k ->
            match pattern with
            | PKeyword -> Some [Keyword k]
            | PKeywordValue k2 when k = k2 -> Some []
            | _ -> None

    let printTree tree = tree |> List.iter (printPatternDecisionTree 0)

    type Matcher(rules: RewriteRule list) =
        let tree = Matcher.getPDT rules

        static member private getNDT (rules: NodeRewriteRule list) : NodeDecisionTree list =
            if rules.Length = 0 then failwith "Got no rules for building a NDT"

            let emptyRules = rules |> List.filter (fun rule -> rule.patternList.Length = 0)
            if emptyRules.Length > 0 then failwithf "Got empty rules: %A" emptyRules

            let ruleGroups = rules |> List.groupBy (fun rule -> rule.patternList.Head)

            let leaves: NodeDecisionTree list = ruleGroups |> List.choose (fun (head, ruleGroup) ->
                let leafRules = ruleGroup |> List.filter (fun rule -> rule.patternList.Length = 1)
                if leafRules.Length = 0 then None else
                // head must be different, but more than one rule matches
                if leafRules.Length > 1 then failwithf "Got multiple leaf rules for the same pattern: %A" leafRules 

                Matcher.getSinglePDT { pattern = head; replacer = leafRules[0].replacer }
                |> NDTLeaf 
                |> Some
            ) 
            
            let nodes: NodeDecisionTree list = ruleGroups |> List.choose (fun (head, rules) ->
                let nodeRules = 
                    rules 
                    |> List.filter (fun rule -> rule.patternList.Length > 1)
                    |> List.map (fun rule -> { rule with patternList = rule.patternList.Tail })
                if nodeRules.Length = 0 then None else

                let pdt = Matcher.getSinglePDT { pattern = head; replacer = fun _ -> failwith "Call to replacer for a pattern that is not fully matched" }
                let ndt = Matcher.getNDT nodeRules
                (pdt, ndt) |> NDTNode |> Some
            )

            // no sorting necessary, as the leaves and nodes are already in order
            leaves@nodes

        static member private getSinglePDT (rule: RewriteRule) =
            let pdts = Matcher.getPDT [rule]
            if pdts.Length <> 1 then failwithf "Expected a single PDT, got %A" pdts else pdts.Head

        // improve partitioning performance here
        static member private getPDT (rules: RewriteRule list) : PatternDecisionTree list =
            if rules.Length = 0 then failwith "Got no rules for building a PDT"

            let leafEdges = 
                rules 
                |> List.filter isLeafPattern
                |> List.map (fun rule -> getLeafPattern rule.pattern, rule.replacer) 
                |> List.map LeafEdge

            let nodeEdge = 
                rules 
                |> List.choose (fun rule -> tryGetChildren rule.pattern |> Option.map (toNodeRewriteRule rule.replacer)) 
                |> fun nodeRules -> if nodeRules.Length = 0 then [] else [Matcher.getNDT nodeRules |> NodeEdge]
            
            let anyEdges =
                rules
                |> List.filter isAnyPattern
                |> List.map (fun rule -> AnyEdge rule.replacer)
            
            if anyEdges.Length > 1 then failwithf "Got multiple rules that match the same pattern (ending with PAny): %A" anyEdges

            leafEdges@nodeEdge@anyEdges |> List.sortBy getPDTSearchPriority

        static member private walkPDT tree argsAcc (expr: Expression) = 
            // LeafEdge should be checked before NodeEdge, but any node must always be matched with NodeEdge, so it's ok
            match expr with
            | Node children ->
                match tree with
                | NodeEdge ndtChildren ->
                    ndtChildren
                    |> List.tryPick (fun childTree -> Matcher.walkNDT childTree argsAcc children)
                | AnyEdge replacer -> Some (replacer, argsAcc@[expr])
                | _ -> 
                    printfn "Backtracking (  node  ) for %A" expr
                    printPatternDecisionTree 1 tree
                    None
            | expr ->
                match tree with
                | LeafEdge(pattern, replacer) ->
                    matchesLeafPattern pattern expr 
                    |> Option.map (fun args -> replacer, args)
                | AnyEdge replacer -> Some (replacer, argsAcc@[expr])
                | _ -> 
                    printfn "Backtracking (not node) for %A:" expr
                    printPatternDecisionTree 1 tree
                    None

        static member private walkNDT tree argsAcc (exprs: Expression list) =
            match tree with
            | NDTLeaf(patternTree) -> 
                if exprs.Length <> 1 then None else
                Matcher.walkPDT patternTree argsAcc exprs[0] 
                |> Option.map (fun treeResult -> treeResult |> fst, argsAcc@(treeResult |> snd))
            | NDTNode(patternTree, childTrees) ->
                Matcher.walkPDT patternTree argsAcc exprs[0]
                |> Option.bind (fun (_, args) ->
                    childTrees
                    |> List.tryPick (fun childTree -> Matcher.walkNDT childTree (argsAcc@args) exprs[1..])
                )

        member this.tryGetReplacementFor expr = 
            tree
            |> List.tryPick (fun childTree -> Matcher.walkPDT childTree [] expr)
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
        printfn "eval: %A" expr
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

let evalProgram p tree = 
    p.freeExpressions |> List.map (Evaluator.eval tree 0)

let runProgram pRaw = 
    evalProgram pRaw (DecisionTree.Matcher pRaw.rewriteRules)