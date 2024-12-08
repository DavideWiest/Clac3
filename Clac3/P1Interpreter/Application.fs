module Clac3.P1Interpreter.Application

open Clac3.Expression
open Clac3.Application
open Clac3.SubstitutionInterpreter
open Clac3.P1Interpreter.Domain
open Clac3.P1Interpreter.BuiltIn
open Clac3.P1Interpreter.DecisionTree

type RewriteRuleApplication(rules: RewriteRule list, expressions: Expression list) =
    inherit Application<RewriteRule, Walker>(rules, expressions)

    override this.eval tree = 
        let tryReplace (tree: Walker) expr = tree.tryReplace expr
        expressions |> List.map (evalExpr (tryReplace tree))

    override this.getEvalArgs = Walker ((Builder rules).constructTree)

type ExtendedRewriteRuleApplication(extraMacros: RewriteRule list, exprs: Expression list) =
    inherit RewriteRuleApplication(List.append coreRuleSet extraMacros, exprs)
