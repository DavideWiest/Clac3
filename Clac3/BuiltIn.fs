module Clac3.BuiltIn

open Clac3.Domain
open Clac3.DomainUtil
open Clac3.Representation

// TODO: rule order? 1) by probability of matching 2) by 
// NOTE: rules should be defined so that all the defined values come first - like in the if-then-else rule
//  the expressions are evaluated depth first from left to right
//  in boolean algebra, it would be a lot more efficient to compute the simpler expressions first
// NOTE: results should not contian References, and strings as References should not be passed to SomePrimLeaf/toprimitive, they will be treated as Strings
// NOTE: rules of dynamic length would be useful too (for arrays, syntax simplification)
// NOTE: rules should be maximally nested to speed up matching
// NOTE: at some point, the expression has to be evaluated. recursive rules should not have a expression-catching block, as they need it evaluated, or it will lead to infinite recursion

// DENESTING
let denestingRules = [
    {
        pattern = Get (TNodeContaining [Get TAny])
        replacement = Args.one (fun item -> item)
    }
]

// CONTROL FLOW
let controlFlowRules = [
    {
        pattern = GetNode ("if", TBool, "then", TAny, "else", TAny)
        replacement = Args.three (fun cond thenExpr elseExpr -> Args.getBool cond |> fun b -> if b then thenExpr else elseExpr)
    }
    {
        pattern = GetNode (TAny, "|>", TNode)
        replacement = Args.two (fun part1 part2 -> Args.getNode part2 |> fun nodePart -> Node (nodePart@[part1]))
    }
]

// LOGIC
let logicRules = [
    {
        pattern = GetNode (TEvaluatedLeaf, "=", TEvaluatedLeaf)
        replacement = Args.two (fun left right -> Bool (left = right))
    }
    {
        pattern = GetNode (TEvaluatedLeaf, "<>", TEvaluatedLeaf)
        replacement = Args.two (fun left right -> Bool (left <> right))
    }
    {
        pattern = GetNode (TBool, "&", TBool)
        replacement = Args.two (fun a b -> Bool (Args.getBool a && Args.getBool b))
    }
    {
        pattern = GetNode (TBool, "|", TBool)
        replacement = Args.two (fun a b -> Bool (Args.getBool a || Args.getBool b))
    }
    {
        pattern = GetNode ("!", TBool)
        replacement = Args.one (fun a -> Bool (not (Args.getBool a)))
    }
]

// REFLECTION
let reflectionRules = [
    {
        pattern = GetNode ("typeof", TEvaluatedLeaf)
        replacement = Args.one (fun expr -> expr.GetType().Name)
    }
    {
        pattern = GetNode ("stringify", TAny)
        replacement = Args.one (fun expr -> ToString.expression expr)
    }
]

let buidArtihmeticRuleSet op opInt opFloat = [
    {
        pattern = GetNode (TInteger, op, TInteger)
        replacement = Args.two (fun a b -> opInt (Args.getInt a) (Args.getInt b))
    }
    {
        pattern = GetNode (TFloat, op, TFloat)
        replacement = Args.two (fun a b -> opFloat (Args.getFloat a) (Args.getFloat b))
    }
    {
        pattern = GetNode (TFloat, op, TInteger)
        replacement = Args.two (fun a b -> opFloat (Args.getFloat a) (float (Args.getInt b)))
    }
    {
        pattern = GetNode (TInteger, op, TFloat)
        replacement = Args.two (fun a b -> opFloat (float (Args.getInt a)) (Args.getFloat b))
    }
]

// ARITHMETIC OPERATIONS
let arithmeticRules = 
    [
        "+", (+), (+)
        "-", (-), (-)
        "*", (*), (*)
        "/", (/), (/)
        "**", pown, ( ** )
    ] 
    |> List.collect (fun (op, opInt, opFloat) -> buidArtihmeticRuleSet op opInt opFloat)

// LISTS
let listRules = [
    {
        pattern = GetNode (TList, "+", TList)
        replacement = Args.two (fun a b -> Args.getNode a @ Args.getNode b)
    }
    {
        pattern = GetNode (TList, "::", TAny)
        replacement = Args.two (fun a b -> (Args.getList a)@[b])
    }
    {
        pattern = GetNode (TAny, "::", TList)
        replacement = Args.two (fun a b -> b::(Args.getList a))
    }
    {
        pattern = GetNode (TList, "map", TNode)
        replacement = Args.two (fun listExpr mapExpr -> 
            Args.getList listExpr 
            |> List.map (fun item -> (Args.getNode mapExpr) @ [item])
            |> List.map node
        )
    }
]

// STRINGS
let stringRules = [
    {
        pattern = GetNode (TString, "+", TString)
        replacement = Args.two (fun a b -> Args.getString a + Args.getString b)
    }
]

let coreRuleSet =
    denestingRules @ controlFlowRules @ logicRules @ reflectionRules @ arithmeticRules @ listRules @ stringRules
