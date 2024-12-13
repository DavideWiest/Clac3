module Clac3.P1.DecisionTree

open Clac3.Util
open Clac3.P1.Expression
open Clac3.P1.Domain

type PatternWrapper<'a> = {
    value: 'a
    any: Replacer option
}

type CollectablePatternWrapper<'a> = {
    value: 'a
    any: Replacer option
}

type MaybePatternWrapper<'a> = PatternWrapper<'a> option
type MaybeCollectablePatternWrapper<'a> = CollectablePatternWrapper<'a> option

// it's a list because there a very few base cases, too few that a hashmap would make sense
// there is no significant performance difference (for now)
type PatternLeaf<'a> when 'a: comparison = PatternWrapper<('a * Replacer) list>
type MaybePatternLeaf<'a> when 'a: comparison = PatternLeaf<'a> option

type AtomDecisionTree = {
    bool: MaybePatternLeaf<bool>
    integer: MaybePatternLeaf<int>
    float: MaybePatternLeaf<float>
    string: MaybePatternLeaf<string>
    variable: MaybePatternLeaf<string>
    keyword: MaybePatternLeaf<string>
}

type NodeDecisionTree = {
    // grouped by same head
    value: (FirstLevelPattern * NodeDecisionTree) list // includes placeholder replacer
    ending: FirstLevelPattern option // includes correct replacer
    rest: Replacer option // like any but includes all the remining children instead of one
}
    
and ExpressionDecisionTree = {
    atom: MaybePatternWrapper<AtomDecisionTree>
    list: MaybeCollectablePatternWrapper<NodeDecisionTree>
    node: MaybeCollectablePatternWrapper<NodeDecisionTree>
}

and FirstLevelPattern = PatternWrapper<ExpressionDecisionTree option>

type TreeResult = (Replacer * Expression list) option

let ifEmptyNoneElseApply f (l: 'a list) = if l.Length = 0 then None else f l |> Some

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
    static member private buildPatternMap (values: (PatternUnion<'a> * Replacer) list) : PatternLeaf<'a> =
        let valueRules, anyRule = Builder.separateAnyFromValueBased "atom" values
        { value = valueRules; any = anyRule }

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
        
    static member private buildAtomInner (rules: (AtomPattern * Replacer) list) : AtomDecisionTree =
        let boolRules, intRules, floatRules, strRules, varRules, kwRules = Builder.partitionAtoms rules

        { 
            bool = ifEmptyNoneElseApply Builder.buildPatternMap boolRules
            integer = ifEmptyNoneElseApply Builder.buildPatternMap intRules
            float = ifEmptyNoneElseApply Builder.buildPatternMap floatRules
            string = ifEmptyNoneElseApply Builder.buildPatternMap strRules
            variable = ifEmptyNoneElseApply Builder.buildPatternMap varRules
            keyword = ifEmptyNoneElseApply Builder.buildPatternMap kwRules
        }

    static member private buildAtom (rules: (PatternUnion<AtomPattern> * Replacer) list) : PatternWrapper<AtomDecisionTree> =
        let valueRules, anyRule = Builder.separateAnyFromValueBased "atom" rules
        { value = Builder.buildAtomInner valueRules; any = anyRule }

    // Lists and Nodes
    static member private buildNodeInner (rules: (CollectablePattern list * Replacer) list) =
        let rulesEnding, rulesContinuing = 
            rules 
            |> List.partition (fun (pattern: CollectablePattern list, _) -> pattern.Length = 1)

        let ending: FirstLevelPattern option = 
            rulesEnding
            |> List.choose (fun (pattern, replacer) -> 
                match pattern.Head with
                | CRest -> None
                | CValue head -> Some (head, replacer)
            )
            |> Builder.getFirstLevelPattern
            |> Some

        let rest: Replacer option =
            rulesEnding
            |> List.filter (fun (pattern, _) -> pattern.Head = CRest)
            |> List.tryHead
            |> Option.map snd
            
        let nodes = 
            rulesContinuing 
            |> List.groupBy (fun (pattern: CollectablePattern list, _) -> pattern.Head) 
            |> List.choose (fun (chead, subRules) ->
                match chead with
                | CRest -> failwith "Rest patterns can only be used at the end of a pattern."
                | CValue head -> 
                    let nodeRules = 
                        subRules 
                        |> List.map (fun (pattern, replacer) -> pattern.Tail, replacer)

                    if nodeRules.Length = 0 then None else

                    let failRule = (head, fun _ -> failwith "Call to replacer for a pattern that's not fully matched")

                    Some (Builder.getFirstLevelPattern [failRule], Builder.buildNodeInner nodeRules)
            )

        { value = nodes; ending = ending; rest = rest }

    static member private buildNode (rules: (PatternUnion<CollectablePattern list> * Replacer) list) =
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

    static member private getFirstLevelPattern (rules: (Pattern * Replacer) list) =
        let valueRules, anyRule = Builder.separateAnyFromValueBased "first level" rules
        { value = ifEmptyNoneElseApply Builder.buildExpression valueRules; any = anyRule }

    member this.constructTree : FirstLevelPattern = 
        allRules |> List.map (fun rule -> rule.pattern, rule.replacer) |> Builder.getFirstLevelPattern


type Walker(tree: FirstLevelPattern) = 
    // Atoms
    static member private tryFindInPatternMap wrapperFn (key: 'a) (patternMap: MaybePatternLeaf<'a>) =
        let withoutKey replacer = replacer, []
        let withKey replacer = replacer, [key |> wrapperFn |> Atom]

        patternMap 
        |> Option.bind (fun pdt -> 
            pdt.value
            |> List.tryFind (fun (k, _) -> k = key)
            |> Option.map snd
            |> Option.map withoutKey
            |> Option.orElse (pdt.any |> Option.map withKey)
        )
    
    static member private tryFindAtomReplacer (tree: AtomDecisionTree) = function
        | Bool b -> tree.bool |> Walker.tryFindInPatternMap Bool b
        | Integer i -> tree.integer |> Walker.tryFindInPatternMap Integer i
        | Float f -> tree.float |> Walker.tryFindInPatternMap Float f
        | String s -> tree.string |> Walker.tryFindInPatternMap String s
        | Variable v -> tree.variable |> Walker.tryFindInPatternMap Variable v
        | Keyword k -> tree.keyword |> Walker.tryFindInPatternMap Keyword k

    static member private walkAtom (atom: Atom) (pattern: PatternWrapper<AtomDecisionTree>) = 
        atom |> Walker.tryFindAtomReplacer pattern.value |> Option.orElse (Option.tupleWithRev pattern.any [Atom atom])

    // Lists and Nodes
    static member private tryGetNodeTreeResultInner wrapperType (children: Expression list) (flp: FirstLevelPattern) (next: NodeDecisionTree) =
        Walker.walk flp children.Head
        |> Option.bind (fun (_, args) -> 
            children.Tail |> Walker.walkNodeInner wrapperType next |> Option.map (fun (replacer, argsTail) -> replacer, (args @ argsTail))
        )

    static member private tryGetNodeTreeResult wrapperType (tree: (FirstLevelPattern * NodeDecisionTree) list) (children: Expression list) = 
        tree |> List.tryPick (fun (flp, next) -> Walker.tryGetNodeTreeResultInner wrapperType children flp next)

    static member private walkNodeInner wrapperType (pattern: NodeDecisionTree) (children: Expression list) = 
        // this order is crucial
        // anything that could cut of another pattern should be check later than that one
        // continuing rules first -- then ending rules -- then rest/collector rules
        if  children.Length > 0 then Walker.tryGetNodeTreeResult wrapperType pattern.value children else None
        |> Option.orElse (if children.Length = 1 then pattern.ending |> Option.bind (fun pattern -> Walker.walk pattern children.Head) else None)
        |> Option.orElse (if children.Length > 0 then pattern.rest |> Option.map (fun replacer -> replacer, [Node children]) else None)

    static member private walkNode wrapperType (children: Expression list) (pattern: CollectablePatternWrapper<NodeDecisionTree>) = 
        children |> Walker.walkNodeInner wrapperType pattern.value |> Option.orElse (Option.tupleWithRev pattern.any [wrapperType children])
        
    // Expressions
    static member private walkExpression tree = function
        | Atom a -> tree.atom |> Option.bind (Walker.walkAtom a)
        | Node children -> tree.node |> Option.bind (Walker.walkNode Node children)
        | List children -> tree.list |> Option.bind (Walker.walkNode List children)

    static member private walk (tree: FirstLevelPattern) (expr: Expression) = 
        tree.value |> Option.bind (fun v -> Walker.walkExpression v expr) |> Option.orElse (Option.tupleWithRev tree.any [expr])
        
    // First level
    member this.tryReplace expr = 
        Walker.walk tree expr |> Option.map (fun (replacer, args) -> replacer args)
      