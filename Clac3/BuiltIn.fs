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
        pattern = PNodeContaining [PAny]
        replacer = Args.one (fun item -> item)
    }
]

// CONTROL FLOW
let controlFlowRules = [
    {
        pattern = pNode ("if", PBool, "then", PAny, "else", PAny)
        replacer = Args.three (fun cond thenExpr elseExpr -> Args.getBool cond |> fun b -> if b then thenExpr else elseExpr)
    }
    {
        pattern = pNode (PAny, "|>", PNode)
        replacer = Args.two (fun part1 part2 -> Args.getNode part2 |> fun nodePart -> Node (nodePart@[part1]))
    }
]

// LOGIC
let logicRules = [
    {
        pattern = pNode (PAnyEvaluatdLeaf, "=", PAnyEvaluatdLeaf)
        replacer = Args.two (fun left right -> Bool (left = right))
    }
    {
        pattern = pNode (PAnyEvaluatdLeaf, "<>", PAnyEvaluatdLeaf)
        replacer = Args.two (fun left right -> Bool (left <> right))
    }
    {
        pattern = pNode (PBool, "&", PBool)
        replacer = Args.two (fun a b -> Bool (Args.getBool a && Args.getBool b))
    }
    {
        pattern = pNode (PBool, "|", PBool)
        replacer = Args.two (fun a b -> Bool (Args.getBool a || Args.getBool b))
    }
    {
        pattern = pNode ("!", PBool)
        replacer = Args.one (fun a -> Bool (not (Args.getBool a)))
    }
]

// REFLECTION
let reflectionRules = [
    {
        pattern = pNode ("typeof", PAnyEvaluatdLeaf)
        replacer = Args.one (fun expr -> expr.GetType().Name)
    }
    {
        pattern = pNode ("stringify", PAny)
        replacer = Args.one (fun expr -> ToString.expression expr)
    }
]

let buidArtihmeticRuleSet op opInt opFloat = [
    {
        pattern = pNode (PInteger, op, PInteger)
        replacer = Args.two (fun a b -> opInt (Args.getInt a) (Args.getInt b))
    }
    {
        pattern = pNode (PFloat, op, PFloat)
        replacer = Args.two (fun a b -> opFloat (Args.getFloat a) (Args.getFloat b))
    }
    {
        pattern = pNode (PFloat, op, PInteger)
        replacer = Args.two (fun a b -> opFloat (Args.getFloat a) (float (Args.getInt b)))
    }
    {
        pattern = pNode (PInteger, op, PFloat)
        replacer = Args.two (fun a b -> opFloat (float (Args.getInt a)) (Args.getFloat b))
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
        pattern = pNode (PList, "+", PList)
        replacer = Args.two (fun a b -> Args.getNode a @ Args.getNode b)
    }
    {
        pattern = pNode (PList, "::", PAny)
        replacer = Args.two (fun a b -> (Args.getList a)@[b])
    }
    {
        pattern = pNode (PAny, "::", PList)
        replacer = Args.two (fun a b -> b::(Args.getList a))
    }
    {
        pattern = pNode (PList, "map", PNode)
        replacer = Args.two (fun listExpr mapExpr -> 
            Args.getList listExpr 
            |> List.map (fun item -> (Args.getNode mapExpr) @ [item])
            |> List.map node
        )
    }
]

// STRINGS
let stringRules = [
    {
        pattern = pNode (PString, "+", PString)
        replacer = Args.two (fun a b -> Args.getString a + Args.getString b)
    }
]

let coreRuleSet =
    denestingRules @ controlFlowRules @ logicRules @ reflectionRules @ arithmeticRules @ listRules @ stringRules
