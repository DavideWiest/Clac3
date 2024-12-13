module Clac3.P2.Application

open Clac3.Application
open Clac3.FExpression
open Clac3.Function
open Clac3.P2.BuiltIn
open Clac3.P2.Interpreter
open Clac3.P2.Normalizer

type FunctionalApplication(funcs: S1.FunctionDefinition array, expressions: S1.FExpression array) =
    inherit Application<(S2.FExpression array) * S2.BindingStore * Map<int, string>, S2.FExpression, FAtom>()

    override this.eval args = 
        let exprs, store, bindingRelation = args
        exprs |> Array.map (evalExpr store) |> Seq.ofArray

    override this.getEvalArgs =
        let bindingMap = funcs |> Array.map (fun f -> f.ident, BFuncDef f) |> Map.ofArray

        normalize expressions bindingMap

type ExtendedFunctionalApplication(extraMacros: S1.FunctionDefinition array, exprs: S1.FExpression array) =
    inherit FunctionalApplication(Array.append coreFunctions extraMacros, exprs)
