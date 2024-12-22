module Clac3.App.ClacApplication

open Clac3.Application
open Clac3.FunctionalExpression
open Clac3.Function
open Clac3.P1.Domain
open Clac3.P2.Interpreter
open Clac3.P2.Normalizer
open Clac3.BuiltIn.P1.Core
open Clac3.BuiltIn.P2.Core
open Clac3.App.P1Application

let private extractBindingExprs definitions = 
    definitions |> Array.map (fun d -> 
        match d.body with
        | RValue expr -> expr
        | RCustom fn -> fn.body
    )

let private toBinding (definition: RawBinding) newExpr = 
    {
        ident = definition.ident
        signature = definition.signature
        binding =
            match definition.body with
            | RValue _ -> BValue newExpr
            | RCustom fn -> Custom { argIdents = fn.argIdents; body = newExpr }
    }

type ClacApplication(rewriteRules, bindings, definitions, exprs) =
    inherit Application<(S2.FExpression array) * S2.BindingStore * Map<int, string>, S2.FExpression, S2.FExpression>()

    override this.eval args = 
        let exprs, store, bindingRelation = args
        exprs |> Array.map (evalFExpr store) |> Seq.ofArray

    override this.getEvalArgs =
        let rawBindingExprs = extractBindingExprs definitions |> List.ofArray

        let p1app = new RewriteRuleApplication(bindings, rewriteRules, definitions, exprs@rawBindingExprs)
        // use the same application and separate the two later to avoid overhead
        // you could create call evalArgs once and use it for the two applications, but that is less reliable, especially later on
        let allExpressions = p1app.eval (p1app.getEvalArgs) |> Seq.toArray
        let expressions, bindingExprs = allExpressions[0..(List.length exprs) - 1], allExpressions[(List.length exprs)..]

        let baseBindingTuple = 
            bindings
            |> Array.map (fun b -> b.ident, b)

        let customBindingTuple = 
            bindingExprs
            |> Array.zip definitions
            |> Array.map (fun (d, newExpr) -> d.ident, toBinding d newExpr)

        let bindingMap = 
            baseBindingTuple 
            |> Array.append customBindingTuple 
            |> Map.ofArray

        normalize expressions bindingMap

type ExtendedClacApplication(extraRules, extraBindings, definitions, exprs) =
    inherit ClacApplication(coreRules@extraRules, Array.append coreBindings extraBindings, definitions, exprs)