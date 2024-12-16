module Clac3.ClacApplication

open Clac3.Application
open Clac3.FunctionalExpression
open Clac3.Function
open Clac3.P1.BuiltIn
open Clac3.P2.Interpreter
open Clac3.P2.Normalizer
open Clac3.P2.BuiltIn

type ClacApplication(rewriteRules, bindings, exprs) =
    inherit Application<(S2.FExpression array) * S2.BindingStore * Map<int, string>, S2.FExpression, S2.FExpression>()

    override this.eval args = 
        let exprs, store, bindingRelation = args
        exprs |> Array.map (evalFExpr store) |> Seq.ofArray

    override this.getEvalArgs =
        let p1app = new Clac3.P1.Application.RewriteRuleApplication(bindings, rewriteRules, exprs)
        let expressions = p1app.eval (p1app.getEvalArgs) |> Seq.toArray
        let bindingMap = bindings |> Array.map (fun b -> b.ident, b) |> Map.ofArray

        normalize expressions bindingMap

type ExtendedClacApplication(extraRules, extraBindings, exprs) =
    inherit ClacApplication(coreRules@extraRules, Array.append coreBindings extraBindings, exprs)