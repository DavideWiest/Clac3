module Clac3.App.P1Application

open Clac3.Application
open Clac3.Expression
open Clac3.Type
open Clac3.P1.DomainUtil
open Clac3.P1.Interpreter
open Clac3.P1.PatternReplacer
open Clac3.P1.DecisionTree.Builder
open Clac3.P1.DecisionTree.Walker
open Clac3.BuiltIn.P1.Core

// uses decision trees. lifts are injected because they are determined by the p2-application
type RewriteRuleApplication(lift: TALifter, rules: RewriteRule list, expressions: Expression list) =
    inherit Application<ValueReplacer * LiftedReplacer, Expression, TAExpression>()

    override this.eval args = 
        let valueReplace, replace = args
  
        let evaledExprs = 
            expressions 
            |> List.map (evalExpr valueReplace replace lift.toTAExpr None) // no expected output type, type annotations not supported yet
            |> Seq.ofList

        evaledExprs

    override this.getEvalArgs = 
        let walker = Walker ((Builder rules).constructTree, lift.toTAExpr)
        
        Interpreter.valueReplacerWrapper walker.tryReplaceValue, Interpreter.replacerWrapper lift.toTAExpr walker.tryReplace

type ExtendedRewriteRuleApplication(lift, extraMacros, exprs) =
    inherit RewriteRuleApplication(lift, coreRules@extraMacros, exprs)