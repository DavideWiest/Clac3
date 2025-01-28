module rec Clac3.P1.DecisionTree.Builder

open Clac3.Util
open Clac3.Type
open Clac3.P1.PatternReplacer
open Clac3.P1.DecisionTree.Domain

let private ifEmptyNoneElseApply f (l: 'a list) = if l.Length = 0 then None else f l |> Some

let private separateSimplePUOptions ruleGroupStr (rules: (SimplePatternUnion<'a> * ProtoReplacer) list) =
    let valueRules = rules |> List.choose (fun (pattern, replacer) -> 
        match pattern with
        | SimplePatternUnion.Value expr -> Some (expr, replacer)
        | _ -> None
    )

    let anyRules = rules |> List.choose (fun (pattern, replacer) -> 
        match pattern with
        | SimplePatternUnion.Any -> Some replacer
        | _ -> None
    )

    if anyRules.Length > 1 then failwithf "Got multiple rules that match the same pattern (any, %s): %A" ruleGroupStr anyRules

    valueRules, List.headOption anyRules

let private separatePUOptions ruleGroupStr (rules: (PatternUnion<'a> * ProtoReplacer) list) =
    let constantValueRules = rules |> List.choose (fun (pattern, replacer) -> 
        match pattern with
        | ConstantValue expr -> Some (expr, replacer)
        | _ -> None
    )

    let valueRules = rules |> List.choose (fun (pattern, replacer) -> 
        match pattern with
        | Value expr -> Some (expr, replacer)
        | _ -> None
    )

    let anyRules = rules |> List.choose (fun (pattern, replacer) -> 
        match pattern with
        | Any -> Some replacer
        | _ -> None
    )

    if anyRules.Length > 1 then failwithf "Got multiple rules that match the same pattern (any, %s): %A" ruleGroupStr anyRules

    constantValueRules, valueRules, List.headOption anyRules

// Atoms
let private buildPatternMap (values: (PatternUnion<'a> * ProtoReplacer) list) : PatternWrapper<('a * ProtoReplacer) list> =
    let constantValueRules, valueRules, anyRule = separatePUOptions "atom" values
    { constantValue = constantValueRules; value = valueRules; any = anyRule }

// TODO: use resizeArray
let private partitionAtoms (rules: (AtomPattern * ProtoReplacer) list) =
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
        
let private buildAtomInner (rules: (AtomPattern * ProtoReplacer) list) : AtomDecisionTree =
    let boolRules, intRules, floatRules, strRules, varRules, kwRules = partitionAtoms rules

    { 
        bool = ifEmptyNoneElseApply buildPatternMap boolRules
        integer = ifEmptyNoneElseApply buildPatternMap intRules
        float = ifEmptyNoneElseApply buildPatternMap floatRules
        string = ifEmptyNoneElseApply buildPatternMap strRules
        variable = ifEmptyNoneElseApply buildPatternMap varRules
        keyword = ifEmptyNoneElseApply buildPatternMap kwRules
    }

let private buildAtom (rules: (PatternUnion<AtomPattern> * ProtoReplacer) list) : PatternWrapper<AtomDecisionTree> =
    let constantValueRules, valueRules, anyRule = separatePUOptions "atom" rules
    { constantValue = buildAtomInner constantValueRules; value = buildAtomInner valueRules; any = anyRule }

// Lists and Nodes
let private buildNodeInner (rules: (CollectablePattern list * ProtoReplacer) list) =
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

    let rest: ProtoReplacer option =
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

let private buildSimplePatternWrapper ruleGroupStr (rules: (SimplePatternUnion<'a> * ProtoReplacer) list) =
    let valueRules, anyRule = separateSimplePUOptions ruleGroupStr rules
    { value = valueRules; any = anyRule }

let private buildNode (rules: (SimplePatternUnion<CollectablePattern list> * ProtoReplacer) list) : CollectablePatternWrapper<NodeDecisionTree> =
    let valueRules, anyRule = separateSimplePUOptions "node" rules
    { value = buildNodeInner valueRules; any = anyRule }

// Expression
let private partitionToExpressionTypes (rules: (ExpressionPattern * ProtoReplacer) list) =
    rules
    |> List.fold (fun (atomRuleSets, nodeRuleSets, arrayRuleSets, lambdaRuleSets) (pattern, replacer) ->
        match pattern with
        | PAtom leaf -> (leaf, replacer)::atomRuleSets, nodeRuleSets, arrayRuleSets, lambdaRuleSets
        | PArray arr -> atomRuleSets, nodeRuleSets, (arr, replacer)::arrayRuleSets, lambdaRuleSets
        | PNode children -> atomRuleSets, (children, replacer)::nodeRuleSets, arrayRuleSets, lambdaRuleSets
        | PLambda signature -> atomRuleSets, nodeRuleSets, arrayRuleSets, (signature, replacer)::lambdaRuleSets
    ) ([], [], [], [])

let private buildExpression (rules: (ExpressionPattern * ProtoReplacer) list) : ExpressionDecisionTree =
    let atomRuleSet, nodeRuleSet, arrayRuleSet, lambdaRuleSets = partitionToExpressionTypes rules

    { 
        atom = ifEmptyNoneElseApply buildAtom atomRuleSet
        node = ifEmptyNoneElseApply buildNode nodeRuleSet
        array = ifEmptyNoneElseApply buildNode arrayRuleSet
        lambda = ifEmptyNoneElseApply (buildSimplePatternWrapper "lambda") lambdaRuleSets
    }

let buildFirstLevel (rules: (Pattern * ProtoReplacer) list) =
    let constantValueRules, valueRules, anyRule = separatePUOptions "first level" rules
    { constantValue = ifEmptyNoneElseApply buildExpression constantValueRules; value = ifEmptyNoneElseApply buildExpression valueRules; any = anyRule }

type Builder(allRules: RewriteRule list) =
    member this.constructTree : FirstLevelPattern = 
        allRules |> List.map (fun rule -> rule.pattern, rule.replacer) |> Builder.buildFirstLevel