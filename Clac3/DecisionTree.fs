module Clac3.DecisionTree

open Clac3.Util
open Clac3.Domain

type PatternWrapper<'a> = {
    value: 'a
    any: Replacer option
}

type MaybePatternWrapper<'a> = PatternWrapper<'a> option

type PatternMap<'a> when 'a: comparison = PatternWrapper<Map<'a, Replacer>>
type MaybePatternMap<'a> when 'a: comparison = PatternMap<'a> option

type LeafDecisionTree = {
    bool: MaybePatternMap<bool>
    integer: MaybePatternMap<int>
    float: MaybePatternMap<float>
    string: MaybePatternMap<string>
    variable: MaybePatternMap<string>
    keyword: MaybePatternMap<string>
}

type NodeDecisionTree = {
    // grouped by same head
    value: (FirstLevelPattern * NodeDecisionTree) list
    ending: Replacer option
    //collector: Replacer option // like any but includes all the remining children instead of one
}
    
and ExpressionDecisionTree = {
    atom: MaybePatternWrapper<LeafDecisionTree>
    list: MaybePatternWrapper<NodeDecisionTree>
    node: MaybePatternWrapper<NodeDecisionTree>
}

and FirstLevelPattern = PatternWrapper<ExpressionDecisionTree option>

type TreeResult = (Replacer * Expression list) option

type NodeRewriteRule = {
    patternList: Pattern list
    replacer: Replacer
}

module ToString = 
    let getPadding indent = String.replicate indent ".   "
    let formatStructNone indent (values: (string * (string option)) list) =
        // printfn "values: %A" values
        values 
        |> List.choose (fun (n,v) -> if v.IsNone then None else Some (sprintf "%s%s\n%s%s" (getPadding indent) n (getPadding indent) v.Value))
        |> String.concat "\n"

    let replacerToString indent (replacer: Replacer option) = replacer |> Option.map (sprintf "%s%A" (getPadding indent))

    let applyToPatternWrapper indent f (patternWrapper: PatternWrapper<'a>) = 
        formatStructNone indent [
            "value", patternWrapper.value |> f |> Some
            "any", replacerToString indent patternWrapper.any
        ]

    let maybePatternMapToString indent (maybePatternMap: MaybePatternMap<'a> when 'a: comparison) =
        match maybePatternMap with
        | Some mapWrapper ->
            formatStructNone indent [
                "map", sprintf "%s%A" (getPadding indent) mapWrapper.value |> Some
                "any", replacerToString indent mapWrapper.any
            ]
            |> Some
        | None -> None

    let leafDecisionTreeToString indent (tree: LeafDecisionTree) =
        formatStructNone indent [
            "bool", maybePatternMapToString (indent+1) tree.bool
            "integer", maybePatternMapToString (indent+1) tree.integer
            "float", maybePatternMapToString (indent+1) tree.float
            "string", maybePatternMapToString (indent+1) tree.string
            "variable", maybePatternMapToString (indent+1) tree.variable
            "keyword", maybePatternMapToString (indent+1) tree.keyword
        ]

    let rec nodeDecisionTreeToString indent (nodeTree: NodeDecisionTree) =
        let valueString =
            nodeTree.value
            |> List.map (fun (pattern, subTree) -> sprintf "%s?\n%s\n%s=>\n%s%s)" (getPadding indent) (firstLevelPatternToString indent pattern) (getPadding indent) (getPadding indent) (nodeDecisionTreeToString (indent+1) subTree))
            |> String.concat (sprintf "\n%s---\n" (getPadding indent))

        formatStructNone indent [
            "value", sprintf "%s" valueString |> Some
            "ending", replacerToString indent nodeTree.ending
        ]

    and expressionDecisionTreeToString indent (exprTree: ExpressionDecisionTree) =
        formatStructNone indent [
            "atom", exprTree.atom |> Option.map (applyToPatternWrapper indent (leafDecisionTreeToString indent))
            "list", exprTree.list |> Option.map (applyToPatternWrapper indent (nodeDecisionTreeToString indent))
            "node", exprTree.node |> Option.map (applyToPatternWrapper indent (nodeDecisionTreeToString indent))
        ]

    and firstLevelPatternToString (indent: int) (pattern: FirstLevelPattern) =
        formatStructNone indent [
            "value", pattern.value |> Option.map (expressionDecisionTreeToString (indent+1))
            "any", replacerToString indent pattern.any
        ]

    let firstLevel pattern = firstLevelPatternToString 0 pattern

let toNodeRewriteRule replacer children = { patternList = children; replacer = replacer }

let ifEmptyNoneElseApply f (l: 'a list) = if l.Length = 0 then None else f l |> Some

// TODO
type Builder(allRules: RewriteRule list) =
    static member private separateAnyFromValueBased ruleGroupStr (rules: (PatternUnion<'a> * Replacer) list) =
        let valueRules = rules |> List.choose (fun (pattern, replacer) -> 
            match pattern with
            | Value expr -> Some (expr, replacer)
            | Any -> None
        )

        let anyRules = rules |> List.choose (fun (pattern, replacer) -> 
            match pattern with
            | Any -> Some replacer
            | _ -> None
        )

        if anyRules.Length > 1 then failwithf "Got multiple rules that match the same pattern (any, %s)" ruleGroupStr

        valueRules, List.headOption anyRules

    // Atoms
    static member private buildPatternMap (values: (PatternUnion<'a> * Replacer) list) : PatternMap<'a> =
        let valueRules, anyRule = Builder.separateAnyFromValueBased "atom" values
        { value = valueRules |> Map.ofList; any = anyRule }

    static member private partitionAtoms (rules: (AtomPattern * Replacer) list) =
        rules
        |> List.fold (fun (boolRules, intRules, floatRules, strRules, varRules, kwRules) (pattern, replacer) -> 
            match pattern with
            | PBool p -> (p, replacer)::boolRules, intRules, floatRules, strRules, varRules, kwRules
            | PInteger p -> boolRules, (p, replacer)::intRules, floatRules, strRules, varRules, kwRules
            | PFloat p -> boolRules, intRules, (p, replacer)::floatRules, strRules, varRules, kwRules
            | PString p -> boolRules, intRules, floatRules, (p, replacer)::strRules, varRules, kwRules
            | PVariable p -> boolRules, intRules, floatRules, strRules, (p, replacer)::varRules, kwRules
            | PKeyword p -> boolRules, intRules, floatRules, strRules, varRules, (p, replacer)::kwRules
        ) ([], [], [], [], [], [])
        
    static member private buildAtomInner (rules: (AtomPattern * Replacer) list) : LeafDecisionTree =
        let boolRules, intRules, floatRules, strRules, varRules, kwRules = Builder.partitionAtoms rules

        { 
            bool = ifEmptyNoneElseApply Builder.buildPatternMap boolRules
            integer = ifEmptyNoneElseApply Builder.buildPatternMap intRules
            float = ifEmptyNoneElseApply Builder.buildPatternMap floatRules
            string = ifEmptyNoneElseApply Builder.buildPatternMap strRules
            variable = ifEmptyNoneElseApply Builder.buildPatternMap varRules
            keyword = ifEmptyNoneElseApply Builder.buildPatternMap kwRules
        }

    static member private buildAtom (rules: (PatternUnion<AtomPattern> * Replacer) list) : PatternWrapper<LeafDecisionTree> =
        let valueRules, anyRule = Builder.separateAnyFromValueBased "atom" rules
        { value = Builder.buildAtomInner valueRules; any = anyRule }

    // Lists and Nodes
    static member private buildNodeInner (rules: (Pattern list * Replacer) list) =

        let rulesEnding, rulesContinuing = List.partition (fun (pattern: Pattern list, _) -> pattern.Length = 0) rules
        if rulesEnding.Length > 1 then failwithf "Got multiple rules that match the same pattern: %A" rulesEnding

        let leaf = 
            rulesEnding 
            |> List.headOption 
            |> Option.map snd

        let nodes = 
            rulesContinuing 
            |> List.groupBy (fun (pattern: Pattern list, _) -> pattern.Head) 
            |> List.choose (fun (head, subRules) ->
                let nodeRules = 
                    subRules 
                    |> List.map (fun (pattern, replacer) -> pattern.Tail, replacer)

                if nodeRules.Length = 0 then None else

                let failRule = (head, fun _ -> failwith "Call to replacer for a pattern that's not fully matched")

                Some (Builder.getFirstLevelPattern [failRule], Builder.buildNodeInner nodeRules)
            )

        { value = nodes; ending = leaf }

    static member private buildNode (rules: (PatternUnion<Pattern list> * Replacer) list) =
        let valueRules, anyRule = Builder.separateAnyFromValueBased "node" rules
        { value = Builder.buildNodeInner valueRules; any = anyRule }

    // Expression
    static member private partitionToExpressionTypes (rules: (ExpressionPattern * Replacer) list) =
        rules
        |> List.fold (fun (atomRuleSets, nodeRuleSets, listRuleSets) (pattern, replacer) ->
            match pattern with
            | PAtom leaf -> (leaf, replacer)::atomRuleSets, nodeRuleSets, listRuleSets
            | PNode children -> atomRuleSets, (children, replacer)::nodeRuleSets, listRuleSets
            | PList children -> atomRuleSets, nodeRuleSets, (children, replacer)::listRuleSets
        ) ([], [], [])

    static member private buildExpression (rules: (ExpressionPattern * Replacer) list) : ExpressionDecisionTree =
        let atomRuleSet, nodeRuleSet, listRuleSet = Builder.partitionToExpressionTypes rules

        { 
            atom = ifEmptyNoneElseApply Builder.buildAtom atomRuleSet
            node = ifEmptyNoneElseApply Builder.buildNode nodeRuleSet
            list = ifEmptyNoneElseApply Builder.buildNode listRuleSet
        }

    static member private getFirstLevelPattern (rules: (PatternUnion<ExpressionPattern> * Replacer) list) =
        let valueRules, anyRule = Builder.separateAnyFromValueBased "first level" rules
        { value = ifEmptyNoneElseApply Builder.buildExpression valueRules; any = anyRule }

    member this.constructTree : FirstLevelPattern = 
        allRules |> List.map (fun rule -> rule.pattern, rule.replacer) |> Builder.getFirstLevelPattern


type Walker(tree: FirstLevelPattern) = 
    // Atoms
    static member private tryFindInPatternMap wrapperFn (key: 'a) (patternMap: MaybePatternMap<'a>) =
        let withKey replacer = replacer, [key |> wrapperFn |> Atom]
        patternMap 
        |> Option.bind (fun pdt -> 
            pdt.value.TryFind key
            |> Option.map withKey
            |> Option.orElse (pdt.any |> Option.map withKey)
        )
                
    static member private tryFindAtomReplacer (tree: LeafDecisionTree) = function
        | Bool b -> tree.bool |> Walker.tryFindInPatternMap Bool b
        | Integer i -> tree.integer |> Walker.tryFindInPatternMap Integer i
        | Float f -> tree.float |> Walker.tryFindInPatternMap Float f
        | String s -> tree.string |> Walker.tryFindInPatternMap String s
        | Variable v -> tree.variable |> Walker.tryFindInPatternMap Variable v
        | Keyword k -> tree.keyword |> Walker.tryFindInPatternMap Keyword k

    static member private tryGetAtomTreeResult (tree: LeafDecisionTree) (atom: Atom) = 
        atom |> Walker.tryFindAtomReplacer tree

    static member private walkAtom (atom: Atom) (pattern: PatternWrapper<LeafDecisionTree>) = 
        atom |> Walker.tryGetAtomTreeResult pattern.value |> Option.orElse (Option.tupleWithRev pattern.any [Atom atom])

    // Lists and Nodes
    static member private tryGetNodeTreeResultInner wrapperType (children: Expression list) (flp: FirstLevelPattern) (next: NodeDecisionTree) =
        Walker.tryGetTreeResult flp children.Head
        |> Option.bind (fun (_, args) -> 
            children.Tail |> Walker.walkNodeInner wrapperType next |> Option.map (fun (replacer, argsTail) -> replacer, (args @ argsTail))
        )

    static member private tryGetNodeTreeResult wrapperType (tree: (FirstLevelPattern * NodeDecisionTree) list) (children: Expression list) = 
        tree |> List.tryPick (fun (flp, next) -> Walker.tryGetNodeTreeResultInner wrapperType children flp next)

    static member private walkNodeInner wrapperType (pattern: NodeDecisionTree) (children: Expression list) = 
        children |> Walker.tryGetNodeTreeResult wrapperType pattern.value |> Option.orElse (Option.tupleWithRev pattern.ending [wrapperType children])

    static member private walkNode wrapperType (children: Expression list) (pattern: PatternWrapper<NodeDecisionTree>) = 
        children |> Walker.walkNodeInner wrapperType pattern.value |> Option.orElse (Option.tupleWithRev pattern.any [wrapperType children])
        
    // Expressions
    static member private walkExpression tree = function
        | Atom a -> tree.atom |> Option.bind (Walker.walkAtom a)
        | Node children -> tree.node |> Option.bind (Walker.walkNode Node children)
        | List children -> tree.list |> Option.bind (Walker.walkNode List children)

    static member private walk (tree: FirstLevelPattern) (expr: Expression) = 
        tree.value |> Option.bind (fun v -> Walker.walkExpression v expr) |> Option.orElse (Option.tupleWithRev tree.any [expr])
        
    // First level
    static member private tryGetTreeResult tree expr : TreeResult = Walker.walk tree expr

    member this.tryReplace expr = Walker.tryGetTreeResult tree expr |> Option.map (fun (replacer, args) -> replacer args)
      