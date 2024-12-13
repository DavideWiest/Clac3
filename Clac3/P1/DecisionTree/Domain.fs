module Clac3.P1.DecisionTree.Domain

open Clac3.P1.Expression
open Clac3.P1.RewriteRule

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
type MaybePatternLeaf<'a> when 'a: comparison = PatternWrapper<('a * Replacer) list> option

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