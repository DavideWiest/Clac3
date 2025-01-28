module Clac3.App.InterpretedApp

open Clac3.Application
open Clac3.FunctionalExpression
open Clac3.Binding
open Clac3.P1.Domain
open Clac3.P2.Conversion
open Clac3.P2.Normalizer
open Clac3.P2.Interpreter
open Clac3.P2.DomainUtil
open Clac3.P2.Lift
open Clac3.BuiltIn.P1.Core
open Clac3.BuiltIn.P2.Core
open Clac3.App.P1Application

let private extractBindingExprs definitions = 
    definitions |> Array.map (fun d -> 
        match d.body with
        | RValue expr -> [d.ident], expr
        | RCustom fn -> [d.ident], fn.body
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

type InterpretedApp(rewriteRules, builtInDefinitions: S1.Binding array, definitions, exprs) =
    inherit Application<(S2.FExpression array) * S2.BindingStore * Map<int, string>, S2.FExpression, S2.FExpression>()

    override this.eval args = 
        let exprs, store, bindingRelation = args
        exprs |> Array.map (evalFExpr store) |> Seq.ofArray

    override this.getEvalArgs =
        let newBindingExprs = extractBindingExprs definitions |> List.ofArray
        let lift = Interpreter.getReferenceStore builtInDefinitions definitions |> functionalLift
        let exprs' = exprs |> List.map (fun e -> [], e)
        let p1app = new RewriteRuleApplication(lift, rewriteRules, exprs'@newBindingExprs)

        // use the same application and separate the two later to avoid overhead
        // you could create call evalArgs once and use it for the two applications, but that is less reliable, especially later on
        let allExpressions = p1app.eval (p1app.getEvalArgs) |> Seq.map (toFunctionalExpressionForTopLevel None) |> Seq.map fst |> Seq.toArray
        let expressions, bindingExprs = allExpressions[0..(List.length exprs) - 1], allExpressions[(List.length exprs)..]

        let liftedDefinitions = 
            bindingExprs
            |> Array.zip definitions
            |> Array.map (fun (d, newExpr) -> toBinding d newExpr)

        let bindingMap = Interpreter.getBindingMap (Array.append builtInDefinitions liftedDefinitions)

        normalize expressions bindingMap

type ExtendedInterpretedApp(extraRules, extraBuiltInDefinitions, definitions, exprs) =
    inherit InterpretedApp(coreRules@extraRules, Array.append coreBindings extraBuiltInDefinitions, definitions, exprs)