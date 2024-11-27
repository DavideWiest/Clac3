module Clac3.Interpreter

open Clac3.Util
open Clac3.Domain

module DecisionTree =
    type ValueDecisionTree<'a> = {
        values: ('a * Replacer) list
        any: Replacer option
    }

    and LeafDecisionTree = {
        bool: ValueDecisionTree<bool> option
        integer: ValueDecisionTree<int> option
        float: ValueDecisionTree<float> option
        string: ValueDecisionTree<string> option
        variable: ValueDecisionTree<string> option
        keyword: ValueDecisionTree<string> option
    }

    and PatternDecisionTree = {
        leaf: ValueDecisionTree<LeafDecisionTree> option
        list: ValueDecisionTree<PatternDecisionTree list> option
        node: ValueDecisionTree<PatternDecisionTree list> option
    }

    type TreeResult = (Replacer * Expression list) option

    type NodeRewriteRule = {
        patternList: Pattern list
        replacer: Replacer
    }

    let toNodeRewriteRule replacer children = { patternList = children; replacer = replacer }

    let matchLeafPattern pattern leaf =
        printfn "MATCHING %A%s WITH %A" leaf (String.replicate (30-leaf.ToString().Length) " ") pattern
        // nested match is more efficient
        // including this in the tree would be more efficient and relatively trivial
        match leaf with
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

    type Matcher(rules: RewriteRule list) =
        let tree = Matcher.getPDT rules

        static member private getNDT (rules: NodeRewriteRule list) =
            if rules.Length = 0 then failwith "Got no rules for building a NDT"

            let emptyRules = rules |> List.filter (fun rule -> rule.patternList.Length = 0)
            if emptyRules.Length > 0 then failwithf "Got empty rules: %A" emptyRules

            let ruleGroups = rules |> List.groupBy (fun rule -> rule.patternList.Head)

            let leaves = ruleGroups |> List.choose (fun (head, ruleGroup) ->
                let leafRules = ruleGroup |> List.filter (fun rule -> rule.patternList.Length = 1)
                if leafRules.Length = 0 then None else
                // head must be different, but more than one rule matches
                if leafRules.Length > 1 then failwithf "Got multiple leaf rules for the same pattern: %A" leafRules 

                Matcher.getSinglePDT { pattern = head; replacer = leafRules[0].replacer }
                |> NDTLeaf 
                |> Some
            ) 
            
            let nodes = ruleGroups |> List.choose (fun (head, rules) ->
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

        static member private getPDTItems (rules: RewriteRule list) =
            let nodeEdge = 
                rules 
                |> List.choose (fun rule -> tryGetChildren rule.pattern |> Option.map (toNodeRewriteRule rule.replacer)) 
                |> List.toOption
                |> Option.map Matcher.getNDT
            
            let anyEdges =
                rules
                |> List.filter isAnyPattern
                |> List.map (fun rule -> rule.replacer)

            let leafEdges = 
                rules 
                |> List.filter isLeafPattern
                |> List.map (fun rule -> getLeafPattern rule.pattern, rule.replacer) 
                |> List.sortBy (fst >> getPDTSearchPriority)
            
            if anyEdges.Length > 1 then failwithf "Got multiple rules that match the same pattern (ending with PAny): %A" anyEdges
            
            nodeEdge, (anyEdges |> List.headOption), leafEdges

        // this is a workaround that should be replaced with a call to a function that matches any rule to a form of PDT
        static member private getSinglePDT (rule: RewriteRule) =
            let nodeEdge, anyEdges, leafEdge = Matcher.getPDTItems [rule]

            nodeEdge 
            |> Option.map NodeEdge
            |> Option.orElse (anyEdges |> Option.map AnyEdge)
            |> Option.orElse (leafEdge |> List.headOption |> Option.map LeafEdge)
            |> fun o -> if o.IsNone then failwith "Got no PDT for a single rule" else o.Value

        static member private getPDT (rules: RewriteRule list) : TopLevelDecisionTree =
            if rules.Length = 0 then failwith "Got no rules for building a PDT"

            let maybeNodeEdge, maybeAnyEdge, leafEdges = Matcher.getPDTItems rules
            
            { 
                MaybeNodeEdge = maybeNodeEdge; 
                MaybeAnyEdge = maybeAnyEdge; 
                LeafEdges = leafEdges 
            }

        static member private walkPDT tree argsAcc = function
            // LeafEdge should be checked before NodeEdge, but any node must always be matched with NodeEdge, so it's ok
            | Node children ->
                match tree with
                | NodeEdge ndtChildren ->
                    ndtChildren
                    |> List.tryPick (Matcher.walkNDT argsAcc children)
                | AnyEdge replacer -> Some (replacer, argsAcc@[Node children])
                | _ -> None
            | expr ->
                match tree with
                | LeafEdge(pattern, replacer) ->
                    matchLeafPattern pattern expr
                    |> Option.map (fun args -> replacer, argsAcc@args)
                | AnyEdge replacer -> Some (replacer, argsAcc@[expr])
                | _ -> None

        static member private walkNDT argsAcc (exprs: Expression list) = function
            | NDTLeaf patternTree -> 
                if exprs.Length <> 1 then None else
                Matcher.walkPDT patternTree argsAcc exprs[0] 
                |> Option.map (fun (replacer, args) -> replacer, argsAcc@args)
            | NDTNode (patternTree, childTrees) ->
                // ignore the replacer. it is just a placeholder
                Matcher.walkPDT patternTree argsAcc exprs[0]
                |> Option.bind (fun (_, args) ->
                    childTrees
                    |> List.tryPick (Matcher.walkNDT (argsAcc@args) exprs[1..])
                )

        member this.tryGetReplacementFor expr = 
            if tree.MaybeAnyEdge.IsSome then tree.MaybeAnyEdge.Value [expr] |> Some else

            match expr with
            | Node children ->
                tree.MaybeNodeEdge
                |> Option.bind (fun ndtChildren -> ndtChildren |> List.tryPick (Matcher.walkNDT [] children))
                |> Option.map (fun (replacer, args) -> 
                    printfn "args: %A" (args |> List.map Representation.ToString.expression)
                    replacer args)
            | leaf -> 
                tree.LeafEdges
                |> List.tryPick (fun (pattern, replacer) -> matchLeafPattern pattern leaf |> Option.map replacer)
      
module Evaluator =
    let private isDefined = function
        | Atom a -> 
            match a with
            | Variable _ -> false
            | _ -> true
        | Node _ -> false

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

    let rec evalExpr (tree: DecisionTree.Matcher) childEvalI (expr: Expression) =
        // if the expression is a leaf but not a variable, it is already evaluated
        if isDefined expr then expr else

        let nextChildEvalI = getNextEvalChildIExpr expr childEvalI
        // breadth first (ie pattern matching on current expr) search if necessary, otherwise depth first (ie evaluating children first) if there are any unevaluated children
        let maybeNewExpr = if isBreadthFirst expr || nextChildEvalI = None then tree.tryGetReplacementFor expr else None

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

module Program =
    let eval (p, tree) = 
        p.freeExpressions |> List.map (Evaluator.evalExpr tree 0)

    let validate p = p

    let getEvalArgs pRaw = 
        validate pRaw, DecisionTree.Matcher pRaw.rewriteRules

    let runProgram = getEvalArgs >> eval