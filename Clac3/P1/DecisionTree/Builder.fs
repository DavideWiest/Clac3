module rec Clac3.P1.DecisionTree.Builder

open Clac3.Util
open Clac3.P1.RewriteRule
open Clac3.P1.DecisionTree.Domain

let private ifEmptyNoneElseApply f (l: 'a list) = if l.Length = 0 then None else f l |> Some

let private separateAnyFromValueBased ruleGroupStr (rules: (PatternUnion<'a> * Replacer) list) =
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
let private buildPatternMap (values: (PatternUnion<'a> * Replacer) list) : PatternLeaf<'a> =
    let valueRules, anyRule = separateAnyFromValueBased "atom" values
    { value = valueRules; any = anyRule }

// TODO: use resizeArray
let private partitionAtoms (rules: (AtomPattern * Replacer) list) =
    rules
    |> List.fold (fun (boolRules, intRules, floatRules, strRules, arrRules, varRules, kwRules) (pattern, replacer) -> 
        match pattern with
        | PBool p -> (p, replacer)::boolRules, intRules, floatRules, strRules, arrRules, varRules, kwRules
        | PInteger p -> boolRules, (p, replacer)::intRules, floatRules, strRules, arrRules, varRules, kwRules
        | PFloat p -> boolRules, intRules, (p, replacer)::floatRules, strRules, arrRules, varRules, kwRules
        | PString p -> boolRules, intRules, floatRules, (p, replacer)::strRules, arrRules, varRules, kwRules
        | PVariable p -> boolRules, intRules, floatRules, strRules, arrRules, (p, replacer)::varRules, kwRules
        | PKeyword p -> boolRules, intRules, floatRules, strRules, arrRules, varRules, (p, replacer)::kwRules
    ) ([], [], [], [], [], [], [])
        
let private buildAtomInner (rules: (AtomPattern * Replacer) list) : AtomDecisionTree =
    let boolRules, intRules, floatRules, strRules, arrRules, varRules, kwRules = partitionAtoms rules

    { 
        bool = ifEmptyNoneElseApply buildPatternMap boolRules
        integer = ifEmptyNoneElseApply buildPatternMap intRules
        float = ifEmptyNoneElseApply buildPatternMap floatRules
        string = ifEmptyNoneElseApply buildPatternMap strRules
        variable = ifEmptyNoneElseApply buildPatternMap varRules
        keyword = ifEmptyNoneElseApply buildPatternMap kwRules
    }

let private buildAtom (rules: (PatternUnion<AtomPattern> * Replacer) list) : PatternWrapper<AtomDecisionTree> =
    let valueRules, anyRule = separateAnyFromValueBased "atom" rules
    { value = buildAtomInner valueRules; any = anyRule }

// Lists and Nodes
let private buildNodeInner (rules: (CollectablePattern list * Replacer) list) =
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
        |> buildFirstLevel
        |> Some

    let restCandidates =
        rulesEnding
        |> List.filter (fun (pattern, _) -> pattern.Head = CRest)

    if restCandidates.Length > 1 then failwithf "Got multiple rules that match the same pattern (rest): %A" restCandidates

    let rest: Replacer option =
        restCandidates
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

                Some (buildFirstLevel [failRule], buildNodeInner nodeRules)
        )

    { value = nodes; ending = ending; rest = rest }

let private buildNode (rules: (PatternUnion<CollectablePattern list> * Replacer) list) =
    let valueRules, anyRule = separateAnyFromValueBased "node" rules
    { value = buildNodeInner valueRules; any = anyRule }

// Expression
let private partitionToExpressionTypes (rules: (ExpressionPattern * Replacer) list) =
    rules
    |> List.fold (fun (atomRuleSets, nodeRuleSets, listRuleSets) (pattern, replacer) ->
        match pattern with
        | PAtom leaf -> (leaf, replacer)::atomRuleSets, nodeRuleSets, listRuleSets
        | PArray arr -> atomRuleSets, nodeRuleSets, (arr, replacer)::listRuleSets
        | PNode children -> atomRuleSets, (children, replacer)::nodeRuleSets, listRuleSets
    ) ([], [], [])

let private buildExpression (rules: (ExpressionPattern * Replacer) list) : ExpressionDecisionTree =
    let atomRuleSet, nodeRuleSet, listRuleSet = partitionToExpressionTypes rules

    { 
        atom = ifEmptyNoneElseApply buildAtom atomRuleSet
        node = ifEmptyNoneElseApply buildNode nodeRuleSet
        list = ifEmptyNoneElseApply buildNode listRuleSet
    }

let buildFirstLevel (rules: (Pattern * Replacer) list) =
    let valueRules, anyRule = separateAnyFromValueBased "first level" rules
    { value = ifEmptyNoneElseApply buildExpression valueRules; any = anyRule }

type Builder(allRules: RewriteRule list) =
    member this.constructTree : FirstLevelPattern = 
        allRules |> List.map (fun rule -> rule.pattern, rule.replacer) |> Builder.buildFirstLevel