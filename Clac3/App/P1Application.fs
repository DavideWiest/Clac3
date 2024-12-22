module Clac3.App.P1Application

open Clac3.FunctionalExpression
open Clac3.Function
open Clac3.Application
open Clac3.Expression
open Clac3.Type
open Clac3.P1.Domain
open Clac3.P1.Interpreter
open Clac3.P1.PatternReplacer
open Clac3.P1.DecisionTree.Builder
open Clac3.P1.DecisionTree.Walker
open Clac3.BuiltIn.P1.Core

let private getEvalArgs (baseBindings: S1.Binding array) rules (definitions: RawBinding array) = 
    let baseSignatureMap = 
        baseBindings 
        |> Array.map (fun b -> b.ident, b.signature) 

    let customSignatureMap =
        definitions 
        |> Array.map (fun d -> d.ident, d.signature)

    let bindingMap = 
        baseSignatureMap
        |> Array.append customSignatureMap
        |> Map.ofArray
    Walker ((Builder rules).constructTree), bindingMap

type RewriteRuleApplication(baseBindings: S1.Binding array, rules: RewriteRule list, definitions: RawBinding array, expressions: Expression list) =
    inherit Application<Walker * S1.SignatureStore, Expression, S1.FExpression>()

    override this.eval args = 
        let tree, bindingStore = args
        let tryReplace expr = tree.tryReplace expr
  
        let evaledExprs = 
            expressions 
            |> List.map (evalExpr bindingStore tryReplace None) // no expected output type, type annotations not supported yet
            |> Seq.ofList

        evaledExprs |> Seq.map fst

    override this.getEvalArgs = getEvalArgs baseBindings rules definitions

type ExtendedRewriteRuleApplication(bindings, extraMacros, definitions, exprs) =
    inherit RewriteRuleApplication(bindings, coreRules@extraMacros, definitions, exprs)

// might be deprecated soon
type StandaloneRewriteRuleApplication(baseBindings, rules, definitions, expressions) =
    inherit Application<Walker * S1.SignatureStore, Expression, TAExpression>()

    override this.eval args = 
        let tree, bindingStore = args
        let tryReplace expr = tree.tryReplace expr
  
        expressions 
        |> List.map (evalExprInner bindingStore tryReplace) // no expected output type, type annotations not supported yet 
        |> Seq.ofList

    override this.getEvalArgs = getEvalArgs baseBindings rules definitions
        