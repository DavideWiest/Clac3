module Clac3.Application

open Clac3.Domain
open Clac3.DecisionTree
open Clac3.Interpreter

type Application = {
    rewriteRules: RewriteRule list;
    freeExpressions: Expression list;
}

module Application =
    let init rules expressions = {
        rewriteRules = rules
        freeExpressions = expressions;
    }

    let eval (p, tree) = 
        p.freeExpressions |> List.map (evalExpr tree 0)

    let validate p = p

    let getEvalArgs pRaw = 
        validate pRaw, Walker ((Builder pRaw.rewriteRules).constructTree)

    let runProgram = getEvalArgs >> eval