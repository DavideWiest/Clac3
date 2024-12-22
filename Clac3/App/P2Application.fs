module Clac3.App.P2Application

open Clac3.FunctionalExpression
open Clac3.Function
open Clac3.Application
open Clac3.BuiltIn.P2.Core
open Clac3.P2.Interpreter
open Clac3.P2.Normalizer

type FunctionalApplication(bindings: S1.Binding array, expressions: S1.FExpression array) =
    inherit Application<(S2.FExpression array) * S2.BindingStore * Map<int, string>, S2.FExpression, S2.FExpression>()

    override this.eval args = 
        let exprs, store, bindingRelation = args
        exprs |> Array.map (evalFExpr store) |> Seq.ofArray

    override this.getEvalArgs =
        let bindingMap = bindings |> Array.map (fun b -> b.ident, b) |> Map.ofArray

        normalize expressions bindingMap

type ExtendedFunctionalApplication(extraBindings: S1.Binding array, exprs: S1.FExpression array) =
    inherit FunctionalApplication(Array.append coreBindings extraBindings, exprs)
