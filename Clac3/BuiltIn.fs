module Clac3.BuiltIn

open Clac3.Domain
open Clac3.DomainUtil
open Clac3.Representation

// NOTE: rules should be defined so that all the defined values come first - like in the if-then-else rule
//  the expressions are evaluated depth first from left to right
//  in boolean algebra, it would be a lot more efficient to compute the simpler expressions first
// NOTE: results should not contian References, and strings as References should not be passed to SomePrimLeaf/toprimitive, they will be treated as Strings
// NOTE: rules of dynamic length would be useful too (for arrays, syntax simplification)
// NOTE: rules should be maximally nested to speed up matching
// NOTE: at some point, the expression has to be evaluated. recursive rules should not have a expression-catching block, as they need it evaluated, or it will lead to infinite recursion

module Helper = 
    let leafAny = [pBo; pInt; pFl; pStr; pLi] // DEPENDENCY: Atom

    let buildArithmeticRuleSetInfixOp op opInt opFloat = [
        {
            pattern = pNC [pInt; vKw op; pInt]
            replacer = Args.two (fun a b -> opInt (Args.getInt a) (Args.getInt b))
        }
        {
            pattern = pNC [pFl; vKw op; pFl]
            replacer = Args.two (fun a b -> opFloat (Args.getFloat a) (Args.getFloat b))
        }
        {
            pattern = pNC [pFl; vKw op; pInt]
            replacer = Args.two (fun a b -> opFloat (Args.getFloat a) (float (Args.getInt b)))
        }
        {
            pattern = pNC [pInt; vKw op; pFl]
            replacer = Args.two (fun a b -> opFloat (float (Args.getInt a)) (Args.getFloat b))
        }
    ]

    let buildEvaluatedValueRuleSetInfixOp op replacement = 
        List.allPairs leafAny leafAny
        |> List.map (fun (a, b) -> 
            { pattern = pNC [a; vKw op; b]; replacer = replacement }
        )

    let buildEvaluatedValueRulePrefixOp op replacement = 
        leafAny
        |> List.map (fun a -> 
            { pattern = pNC [vKw op; a]; replacer = replacement }
        )

// DENESTING
let denestingRules = [
    {
        pattern = pNC [Any]
        replacer = (Args.one (fun item -> item))
    }
]

// CONTROL FLOW
let controlFlowRules = [
    {
        pattern = pNC [vKw "if"; pBo; vKw "then"; Any; vKw "else"; Any]
        replacer = Args.three (fun cond thenExpr elseExpr -> if Args.getBool cond then thenExpr else elseExpr)
    }
]

// LOGIC
let logicRules = 
    Helper.buildEvaluatedValueRuleSetInfixOp "=" (Args.two (fun left right -> aBo (left = right)))
    @ Helper.buildEvaluatedValueRuleSetInfixOp "<>" (Args.two (fun left right -> aBo (left <> right)))

// BOOLEAN ALGEBRA
let booleanRules = [
    {
        pattern = pNC [pBo; vKw "&"; pBo]
        replacer = Args.two (fun a b -> aBo (Args.getBool a && Args.getBool b))
    }
    {
        pattern = pNC [pBo; vKw "|"; pBo]
        replacer = Args.two (fun a b -> aBo (Args.getBool a || Args.getBool b))
    }
    {
        pattern = pNC [vKw "!"; pBo]
        replacer = Args.one (fun a -> aBo (not (Args.getBool a)))
    }
]

// REFLECTION
let reflectionRules = 
    Helper.buildEvaluatedValueRulePrefixOp "typeof" (Args.one (fun expr -> aStr (expr.GetType().Name))) 
    @ [
    {
        pattern = pNC [vKw "stringify"; Any]
        replacer = Args.one (fun expr -> expr |> ToString.expression |> aStr)
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
    |> List.collect (fun (op, opInt, opFloat) -> Helper.buildArithmeticRuleSetInfixOp op (fun a b -> opInt a b |> aInt) (fun a b -> opFloat a b |> aFl))

// LISTS
let listRules = 
    [
        {
            pattern = pNC [pLi; vKw "+"; pLi]
            replacer = Args.two (fun a b -> Args.getList a @ Args.getList b |> List)
        }
        {
            pattern = pNC [pLi; vKw "map"; pNo]
            replacer = Args.two (fun listExpr mapExpr -> 
                Args.getList listExpr
                |> List.map (fun item -> Args.getNode mapExpr @ [item])
                |> List.map Node
                |> List
            )
        }
        {
            pattern = pNC [pLi; vKw "::"; Any]
            replacer = Args.two (fun a b -> Args.getList a @ [b] |> List)
        }
        {
            pattern = pNC [Any; vKw "::"; pLi]
            replacer = Args.two (fun a b -> b :: Args.getList a |> List)
        }
    ]

// STRINGS
let stringRules = [
    {
        pattern = pNC [pStr; vKw "+"; pStr]
        replacer = Args.two (fun a b -> Args.getString a + Args.getString b |> aStr)
    }
]

let functionalCompositionRules = [
    {
        pattern = pNC [Any; vKw "|>"; pNo]
        replacer = Args.two (fun part1 part2 -> (Args.getNode part2)@[part1] |> aNo)
    }
]

let coreRuleSet =
    denestingRules @ logicRules @ booleanRules @ reflectionRules @ arithmeticRules @ listRules @ stringRules @ functionalCompositionRules
    // branching rules won't work because the substituioninterpreter is depth-first now
    // @ controlFlowRules 
    