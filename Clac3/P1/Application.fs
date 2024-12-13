module Clac3.P1.Application

open Clac3.Expression
open Clac3.Application
open Clac3.P1.SubstitutionInterpreter
open Clac3.P1.RewriteRule
open Clac3.P1.DecisionTree.Builder
open Clac3.P1.DecisionTree.Walker
open Clac3.P1.BuiltIn

type RewriteRuleApplication(rules: RewriteRule list, expressions: Expression list) =
    inherit Application<Walker, Expression, Expression>()

    override this.eval tree = 
        let tryReplace expr = tree.tryReplace expr
        expressions |> List.map (evalExpr tryReplace id) |> Seq.ofList

    override this.getEvalArgs = Walker ((Builder rules).constructTree)

type ExtendedRewriteRuleApplication(extraMacros: RewriteRule list, exprs: Expression list) =
    inherit RewriteRuleApplication(coreRuleSet@extraMacros, exprs)
