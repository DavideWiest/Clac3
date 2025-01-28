module rec Clac3.P1.DecisionTree.Walker

open Clac3.Util
open Clac3.Expression
open Clac3.Type
open Clac3.P1.PatternReplacer
open Clac3.P1.DecisionTree.Domain


type Walker(tree: FirstLevelPattern, lift: TALift) = 
    // Atoms
    member private this.tryFindInPatternMap (maybeKey: 'a option) (expr) (patternMap: MaybePatternLeaf<'a>) =
        let withoutKey replacer = replacer, []
        let withKey replacer = replacer, [expr]
        let tryFindInPatterns key = List.tryFind (fun (k, _) -> k = key) >> Option.map snd >> Option.map withoutKey

        patternMap 
        |> Option.bind (fun pdt -> 
            maybeKey 
            |> Option.bind (fun key ->
                tryFindInPatterns key pdt.constantValue
                |> Option.orElse (tryFindInPatterns key pdt.value)
            )
            |> Option.orElse (pdt.any |> Option.map withKey)
        )
    
    member private this.tryFindAtomReplacer tree atomExpr = function
        | Bool b -> tree.bool |> this.tryFindInPatternMap (Some b) atomExpr
        | Integer i -> tree.integer |> this.tryFindInPatternMap (Some i) atomExpr
        | Float f -> tree.float |> this.tryFindInPatternMap (Some f) atomExpr
        | String s -> tree.string |> this.tryFindInPatternMap (Some s) atomExpr
        | Variable _ -> 
            this.tryFindAtomReplacerAsUnevaluated atomExpr tree // use the variable type to find the replacer
        | Keyword k -> tree.keyword |> this.tryFindInPatternMap (Some k) atomExpr

    member private this.walkAtom atomExpr atom (pattern: PatternWrapper<AtomDecisionTree>) = 
        atom
        |> this.tryFindAtomReplacer pattern.value atomExpr
        |> Option.orElse (Option.tupleWithRev pattern.any [atomExpr])

    // Atoms (as nodes, through types)

    member private this.tryFindAtomReplacerAsUnevaluated nodeExpr (tree: AtomDecisionTree) : TreeResult =
        match nodeExpr.eType with
        | TBool -> tree.bool |> this.tryFindInPatternMap None nodeExpr
        | TInteger -> tree.integer |> this.tryFindInPatternMap None nodeExpr
        | TFloat -> tree.float |> this.tryFindInPatternMap None nodeExpr
        | TString -> tree.string |> this.tryFindInPatternMap None nodeExpr
        | TKeyword -> tree.keyword |> this.tryFindInPatternMap None nodeExpr
        | _ -> None

    member private this.tryFindReplacerAsNode tree nodeExpr : TreeResult =
        match nodeExpr.eType with
        | TBool | TInteger | TFloat | TString | TKeyword -> 
            tree.atom 
            |> Option.bind (fun pattern -> 
                pattern.value 
                |> this.tryFindAtomReplacerAsUnevaluated nodeExpr
                |> Option.orElse (Option.tupleWithRev pattern.any [nodeExpr])
            )
        | TArray _ ->
            tree.array 
            |> Option.bind (fun p -> p.value.rest |> Option.orElse p.any )
            |> Option.tupleWith [nodeExpr]
            |> Option.reverseTuple
        | TLambda signature -> 
            tree.lambda 
            |> Option.bind (fun p -> 
                p.value
                |> List.tryFind (fun (k, _) -> k = signature)
                |> Option.map snd
                |> Option.orElse p.any
                |> Option.tupleWith [nodeExpr] 
                |> Option.reverseTuple
            )
        | TUnit -> 
            tree.node 
            |> Option.bind (fun p -> p.any) 
            |> Option.tupleWith [nodeExpr]
            |> Option.reverseTuple

    member this.walkAtomAsNode nodeExpr (tree: ExpressionDecisionTree) =
        nodeExpr |> this.tryFindReplacerAsNode tree

    // Arrays and Nodes
    member private this.tryGetNodeTreeResultInner ctxPath (children: TAExpression list) (flp: FirstLevelPattern, next: NodeDecisionTree) =
        this.walk ctxPath flp children.Head     
        |> Option.bind (fun (_, args) -> 
            next
            |> this.walkNodeInner ctxPath children.Tail 
            |> Option.map (fun (replacer, argsTail) -> replacer, args@argsTail)
        )

    member private this.walkNodeInner ctxPath (children: TAExpression list) pattern = 
        // this order is crucial
        // anything that could cut of another pattern should be check later than that one
        // continuing rules first -- then ending rules -- then rest/collector rules
        if children.Length > 0 then List.tryPick (this.tryGetNodeTreeResultInner ctxPath children) pattern.value else None
        |> Option.orElse (if children.Length = 1 then pattern.ending |> Option.bind (fun pattern -> this.walk ctxPath pattern children.Head) else None) // information loss here
        // dont check for rest if there are no children - this is the base case that should be a separate rule/pattern
        |> Option.orElse (if children.Length > 0 then pattern.rest |> Option.map (fun replacer -> replacer, children) else None)

    member private this.walkNode ctxPath (maybeExpr: TAExpression option) (children: TAExpression list) (pattern: CollectablePatternWrapper<NodeDecisionTree>) = 
        pattern.value  |> this.walkNodeInner ctxPath children |> Option.orElse (if maybeExpr.IsSome then Option.tupleWithRev pattern.any [maybeExpr.Value] else None)

    // Expressions
    member private this.walkExpressionValue ctxPath tree = function
        | TAAtom a -> 
            tree.atom |> Option.bind (this.walkAtom (a |> TAAtom |> lift ctxPath) a)
        | TAArray items -> 
            tree.array |> Option.bind (this.walkNode ctxPath (items |> TAArray |> lift ctxPath |> Some) items)
        | TANode children -> 
            tree.node |> Option.bind (this.walkNode ctxPath None children)

    member private this.walkExpression ctxPath tree expr : TreeResult = 
        // the method still needs to check every part because it's called by the value-methods too
        match expr.expr with
        | TAAtom a -> tree.atom |> Option.bind (this.walkAtom expr a)
        | TAArray children -> tree.array |> Option.bind (this.walkNode ctxPath (Some expr) children)
        | TANode children -> 
            tree.node 
            |> Option.bind (this.walkNode ctxPath (Some expr) children)
            |> Option.orElse (this.walkAtomAsNode expr tree)

    // First level
    member this.walkValue ctxPath (tree: FirstLevelPattern) (expr: TAExpressionValue) : TreeResult = 
        tree.value |> Option.bind (fun v -> this.walkExpressionValue ctxPath v expr) 

    member this.walk ctxPath (tree: FirstLevelPattern) (expr: TAExpression) : TreeResult = 
        tree.value 
        |> Option.bind (fun v -> this.walkExpression ctxPath v expr) 
        |> Option.orElse (Option.tupleWithRev tree.any [expr])

    // replacing
    member this.tryReplaceValue ctxPath = 
        this.walkValue ctxPath tree >> Option.map (fun (replacer, args) -> replacer args)
        //(fun x -> 
        //    printfn "ReplaceValue: %A" x 
        //    x) >> this.walkValue ctxPath tree >> Option.map (fun (replacer, args) -> 
        //        printfn "Match: %A(%A) -> %A" replacer args (replacer args)
        //        replacer args)

    member this.tryReplace ctxPath = 
        this.walk ctxPath tree >> Option.map (fun (replacer, args) -> replacer args)
        //(fun x -> 
        //    printfn "Replace: %A" x 
        //    x) >> this.walk ctxPath tree >> Option.map (fun (replacer, args) -> 
        //        printfn "Match: \n    %A(%A) \n->\n    %A" replacer args (replacer args)
        //        replacer args)
