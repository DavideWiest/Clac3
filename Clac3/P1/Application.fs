module Clac3.P1.Application

open Clac3.Expression
open Clac3.FunctionalExpression
open Clac3.Function
open Clac3.Application
open Clac3.P1.SubstitutionInterpreter
open Clac3.P1.RewriteRule
open Clac3.P1.DecisionTree.Builder
open Clac3.P1.DecisionTree.Walker
open Clac3.P1.BuiltIn

type RewriteRuleApplication(rules: RewriteRule list, expressions: Expression list, bindingStore: S1.BindingStore) =
    inherit Application<Walker, Expression, S1.FExpression>()

    override this.eval tree = 
        let tryReplace expr = tree.tryReplace expr
  
        expressions 
        |> List.map (evalExpr tryReplace bindingStore None) // no expected output type, type annotations not supported yet 
        |> Seq.ofList

    override this.getEvalArgs = Walker ((Builder rules).constructTree)

type ExtendedRewriteRuleApplication(extraMacros, exprs, bindingStore) =
    inherit RewriteRuleApplication(coreRuleSet@extraMacros, exprs, bindingStore)
