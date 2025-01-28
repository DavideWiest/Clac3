module Clac3.P1.DecisionTree.Domain

open Clac3.Type
open Clac3.P1.PatternReplacer

type PatternWrapper<'a> = {
    constantValue: 'a
    value: 'a
    any: ProtoReplacer option
}

type CollectablePatternWrapper<'a> = {
    // no constant value for now, would only apply to arrays
    value: 'a
    any: ProtoReplacer option
}

type MaybePatternWrapper<'a> = PatternWrapper<'a> option
type MaybeCollectablePatternWrapper<'a> = CollectablePatternWrapper<'a> option

type SimplePatternWrapper<'a> = {
    value: 'a
    any: ProtoReplacer option
}

type MaybeSimplePatternWrapper<'a> = SimplePatternWrapper<'a> option

// it's a list because there a very few base cases, too few that a hashmap would make sense
// there is no significant performance difference (for now)
type MaybePatternLeaf<'a> when 'a: comparison = PatternWrapper<('a * ProtoReplacer) list> option

type AtomDecisionTree = {
    bool: MaybePatternLeaf<bool>
    integer: MaybePatternLeaf<int>
    float: MaybePatternLeaf<float>
    string: MaybePatternLeaf<string>
    variable: MaybePatternLeaf<string>
    keyword: MaybePatternLeaf<string>
}

// matching empty arrays/nodes is not yet supported
type NodeDecisionTree = {
    // grouped by same head
    value: (FirstLevelPattern * NodeDecisionTree) list // includes placeholder replacer
    ending: FirstLevelPattern option // includes correct replacer
    rest: ProtoReplacer option // like any but includes all the remining children instead of one
}

and LambdaDecisionTree = (FnSignature * ProtoReplacer) list

and ExpressionDecisionTree = {
    atom: MaybePatternWrapper<AtomDecisionTree>
    array: MaybeCollectablePatternWrapper<NodeDecisionTree>
    node: MaybeCollectablePatternWrapper<NodeDecisionTree>
    lambda: MaybeSimplePatternWrapper<LambdaDecisionTree>
}

and FirstLevelPattern = PatternWrapper<ExpressionDecisionTree option>

type TreeResult = (ProtoReplacer * (TAExpression list)) option