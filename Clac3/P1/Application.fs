module Clac3.P1.Application

open Clac3.FunctionalExpression
open Clac3.Function
open Clac3.Application
open Clac3.Expression
open Clac3.Type
open Clac3.P1.Interpreter
open Clac3.P1.RewriteRule
open Clac3.P1.DecisionTree.Builder
open Clac3.P1.DecisionTree.Walker
open Clac3.P1.BuiltIn

type RewriteRuleApplication(baseBindings: S1.Binding array, rules: RewriteRule list, expressions: Expression list) =
    inherit Application<Walker * S1.BindingStore, Expression, S1.FExpression>()

    override this.eval args = 
        let tree, bindingStore = args
        let tryReplace expr = tree.tryReplace expr
  
        let evaledExprs = 
            expressions 
            |> List.map (evalExpr bindingStore tryReplace None) // no expected output type, type annotations not supported yet
            |> Seq.ofList

        evaledExprs |> Seq.map fst

    override this.getEvalArgs = 
        let bindingMap = baseBindings |> Array.map (fun b -> b.ident, b) |> Map.ofArray
        Walker ((Builder rules).constructTree), bindingMap

type ExtendedRewriteRuleApplication(bindings, extraMacros, exprs) =
    inherit RewriteRuleApplication(bindings, coreRules@extraMacros, exprs)

type StandaloneRewriteRuleApplication(baseBindings: S1.Binding array, rules: RewriteRule list, expressions: Expression list) =
    inherit Application<Walker * S1.BindingStore, Expression, TAExpression>()

    override this.eval args = 
        let tree, bindingStore = args
        let tryReplace expr = tree.tryReplace expr
  
        expressions 
        |> List.map (evalExprInner bindingStore tryReplace) // no expected output type, type annotations not supported yet 
        |> Seq.ofList

    override this.getEvalArgs = 
        let bindingMap = baseBindings |> Array.map (fun b -> b.ident, b) |> Map.ofArray
        Walker ((Builder rules).constructTree), bindingMap