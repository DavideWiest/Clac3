module Clac3.BuiltIn.P1.Core

open Clac3.Expression
open Clac3.P1.DomainUtil
open Clac3.P1.PatternReplacer

// NOTE: rules should be defined so that all the defined values come first - like in the if-then-else rule
//  the expressions are evaluated depth first from left to right
//  in boolean algebra, it would be a lot more efficient to compute the simpler expressions first
// NOTE: results should not contian References, and strings as References should not be passed to SomePrimLeaf/toprimitive, they will be treated as Strings
// NOTE: rules of dynamic length would be useful too (for arrays, syntax simplification)
// NOTE: rules should be maximally nested to speed up matching
// NOTE: at some point, the expression has to be evaluated. recursive rules should not have a expression-catching block, as they need it evaluated, or it will lead to infinite recursion

module Helper = 
    let leafAny = [pBo; pInt; pFl; pStr; pLi] // DEPENDENCY: Atom
    let allLeafsAnnotated = [(pBo, "Bool"); (pInt, "Int"); (pFl, "Float"); (pStr, "String"); (pLi, "List")]
    let numbericLeafAnnotated = [(pInt, "Int"); (pFl, "Float")]
    let booleanLeafAnnotated = [(pBo, "Bool")]

    let buildArithmeticRuleSetInfixOp op opInt opFloat = [
        {
            pattern = NC [pInt; vKw op; pInt]
            replacer = Args.two (fun a b -> Node [aKw opInt; a; b])
        }
        {
            pattern = NC [pFl; vKw op; pFl]
            replacer = Args.two (fun a b -> Node [aKw opFloat; a; b])
        }
    ]

    let buildEvaluatedValueRuleSetInfixOp op replacement = 
        List.allPairs leafAny leafAny
        |> List.map (fun (a, b) -> 
            { pattern = NC [a; vKw op; b]; replacer = replacement }
        )

    let buildEvaluatedValueRulePrefixOp op replacement = 
        leafAny
        |> List.map (fun a -> 
            { pattern = NC [vKw op; a]; replacer = replacement }
        )

    let buildTypeSpecificationsTwoArgs ident patternsWithStringAnnotations = 
        patternsWithStringAnnotations
        |> List.map (fun (p,typeAnnotation) -> 
            { pattern = NC [vKw ident; p; p]; replacer = Args.two(fun a b -> Node [aKw (ident + typeAnnotation); a; b]) }
        )

// DENESTING
let denestingRule = {
    pattern = NC [Any]
    replacer = Args.one (fun item -> item)
}

// flatten the node, so that the first item is always the identifier
let flattenRule = {
    pattern = NCC [pNo]
    replacer = Args.two (fun a rest -> Node (Args.getChildren(a)@Args.getChildren(rest)))
}

// CONTROL FLOW
let controlFlowRules = [
    {
        pattern = NC [vKw "if"; pBo; vKw "then"; Any; vKw "else"; Any]
        // this should not be replaced by branchIdent, because it-then-else is too specific
        replacer = Args.three (fun cond thenExpr elseExpr -> Node [aKw "ifthenelse"; cond; thenExpr; elseExpr])
    }
    // TODO: match
]

// LOGIC
let equalityRules = [
    {
        pattern = NC [Any; vKw "="; Any]
        replacer = Args.two (fun left right -> Node [aKw "eq"; left; right])
    }
    {
        pattern = NC [Any; vKw "!="; Any]
        replacer = Args.two (fun left right -> Node [aKw "not"; Node [aKw "eq"; left; right]])
    }
    // TODO: greater, less, greaterOrEqual, lessOrEqual
]

// BOOLEAN ALGEBRA
let booleanRules = [
    {
        pattern = NCC [pBo; vKw "&"]
        replacer = Args.two (fun a rest -> Node [aKw "and"; a; rest])
    }
    {
        pattern = NCC [pBo; vKw "|"]
        replacer = Args.two (fun a rest -> Node [aKw "or"; a; rest])
    }
    {
        pattern = NC [vKw "!"; pBo]
        replacer = Args.one (fun a -> Node [aKw "not"; a])
    }
]

// ARITHMETIC OPERATIONS
let arithmeticRules = 
    [
        "+", "add"
        "-", "substract"
        "*", "mul"
        "/", "div"
        "**", "pow"
    ] 
    |> List.collect (fun (op, fnName) -> Helper.buildArithmeticRuleSetInfixOp op (fnName + "Int") (fnName + "Float"))

// LISTS
let listRules = [
    {
        pattern = NCC [pLi; vKw "+"]
        replacer = Args.two (fun l rest -> Node [aKw "List.append"; l; rest])
    }
    {
        pattern = NC [pLi; vKw "map"; pNo]
        replacer = Args.two (fun listExpr mapExpr -> Node [aKw "List.map"; listExpr; mapExpr])
    }
    // TODO: cons operator, concat, filter, fold, reduce, zip
]

// STRINGS
let stringRules = [
    {
        pattern = NCC [pStr; vKw "+"]
        replacer = Args.two (fun s rest -> Node [aKw "String.append"; s; rest])
    }
    // TODO: split, join, replace, trim, toUpper, toLower, startsWith, endsWith, contains, indexOf, lastIndexOf, substring, length
]

let functionalCompositionRules = [
    {
        pattern = NCC [Any; vKw "|>"; Any]
        replacer = Args.two (fun input func-> Node [func; input]) // func will be flattened later if it's a node
    }
    // TODO: pipeBackward, compose, flip, lambda syntax
]

let typeSpecificationRules = 
    Helper.buildTypeSpecificationsTwoArgs "eq" Helper.allLeafsAnnotated @

    Helper.buildTypeSpecificationsTwoArgs "add" Helper.numbericLeafAnnotated @
    Helper.buildTypeSpecificationsTwoArgs "substract" Helper.numbericLeafAnnotated @
    Helper.buildTypeSpecificationsTwoArgs "multiply" Helper.numbericLeafAnnotated @
    Helper.buildTypeSpecificationsTwoArgs "divide" Helper.numbericLeafAnnotated

let coreRules =
    // denestingRule first, as it's the most likely to be applied
    // flattenRule last so that all other rules (of the same pattern) are applied first
    denestingRule::controlFlowRules @ equalityRules @ booleanRules @ arithmeticRules @ 
    // listRules @ stringRules @ functionalCompositionRules @
    [flattenRule] 
    