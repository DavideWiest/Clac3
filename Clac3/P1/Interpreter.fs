module Clac3.P1.Interpreter

open Clac3.Expression
open Clac3.Type
open Clac3.P1.PatternReplacer
open Clac3.P1.DomainUtil

let rec evalExprInner valueReplace liftedReplace lift expr =
    let evalItems = List.map (evalExprInner valueReplace liftedReplace lift)
    let evaluate = function
        | Atom a -> TAAtom a
        | Array items -> 
            items |> evalItems |> TAArray
        | Node children ->
            children |> evalItems |> TANode

    // the first replacement round ensures mutual syntax
    // the second replacement round is for matching expressions with an unevaluated node (but known type) to patterns
    expr |> evaluate |> valueReplace |> lift |> liftedReplace

let evalExpr valueReplace replace (lift: TALift) ctxPath expectedOutputType = evalExprInner valueReplace replace (lift ctxPath) >> Interpreter.compareType expectedOutputType