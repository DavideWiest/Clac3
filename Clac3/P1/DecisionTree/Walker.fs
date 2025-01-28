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
                |> Option.orElse (pdt.any |> Option.map withKey)
            )
        )
    
    member private this.tryFindAtomReplacer tree atomExpr atom =
        match atom with
        | Bool b -> tree.bool |> this.tryFindInPatternMap (Some b) atomExpr
        | Integer i -> tree.integer |> this.tryFindInPatternMap (Some i) atomExpr
        | Float f -> tree.float |> this.tryFindInPatternMap (Some f) atomExpr
        | String s -> tree.string |> this.tryFindInPatternMap (Some s) atomExpr
        | Variable v -> this.tryFindAtomReplacerAsUnevaluated atomExpr tree // use the variable type to find the replacer
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
    member private this.tryGetNodeTreeResultInner (children: TAExpression list) (flp: FirstLevelPattern, next: NodeDecisionTree) =
        this.walk flp children.Head
        |> Option.bind (fun (_, args) -> 
            next
            |> this.walkNodeInner children.Tail 
            |> Option.map (fun (replacer, argsTail) -> replacer, args@argsTail)
        )

    member private this.walkNodeInner (children: TAExpression list) pattern = 
        // this order is crucial
        // anything that could cut of another pattern should be check later than that one
        // continuing rules first -- then ending rules -- then rest/collector rules
        if children.Length > 0 then List.tryPick (this.tryGetNodeTreeResultInner children) pattern.value else None
        |> Option.orElse (if children.Length = 1 then pattern.ending |> Option.bind (fun pattern -> this.walk pattern children.Head) else None) // information loss here
        // dont check for rest if there are no children - this is the base case that should be a separate rule/pattern
        |> Option.orElse (if children.Length > 0 then pattern.rest |> Option.map (fun replacer -> replacer, children) else None)

    member private this.walkNode (maybeExpr: TAExpression option) (children: TAExpression list) (pattern: CollectablePatternWrapper<NodeDecisionTree>) = 
        pattern.value  |> this.walkNodeInner children |> Option.orElse (if maybeExpr.IsSome then Option.tupleWithRev pattern.any [maybeExpr.Value] else None)

    // Expressions
    member private this.walkExpression tree expr : TreeResult = 
        match expr.expr with
        | TAAtom a -> tree.atom |> Option.bind (this.walkAtom expr a)
        | TAArray children -> tree.array |> Option.bind (this.walkNode (Some expr) children)
        | TANode children -> 
            tree.node 
            |> Option.bind (this.walkNode (Some expr) children)
            |> Option.orElse (this.walkAtomAsNode expr tree)

    member private this.walkExpressionValue tree = function
        | TAAtom a -> 
            tree.atom |> Option.bind (this.walkAtom (lift (TAAtom a)) a)
        | TAArray items -> 
            tree.array |> Option.bind (this.walkNode (items |> TAArray |> lift |> Some) items)
        | TANode children -> 
            tree.node |> Option.bind (this.walkNode None children)

    // First level
    member this.walk (tree: FirstLevelPattern) (expr: TAExpression) : TreeResult = 
        tree.value 
        |> Option.bind (fun v -> this.walkExpression v expr) 
        |> Option.orElse (Option.tupleWithRev tree.any [expr])

    member this.walkValue (tree: FirstLevelPattern) (expr: TAExpressionValue) : TreeResult = 
        tree.value |> Option.bind (fun v -> this.walkExpressionValue v expr) 

    // replacing
    member this.tryReplaceValue = 
        // this.walkValue tree >> Option.map (fun (replacer, args) -> replacer args)
        (fun x -> 
            printfn "ReplaceValue: %A" x 
            x) >> this.walkValue tree >> Option.map (fun (replacer, args) -> 
                printfn "Match: %A(%A) -> %A" replacer args (replacer args)
                replacer args)

    member this.tryReplace = 
        // this.walk tree >> Option.map (fun (replacer, args) -> replacer args)
        (fun x -> 
            printfn "Replace: %A" x 
            x) >> this.walk tree >> Option.map (fun (replacer, args) -> 
                printfn "Match: \n    %A(%A) \n->\n    %A" replacer args (replacer args)
                replacer args)