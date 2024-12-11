module Clac3.P2.BuiltIn

open Clac3.P2.FExpression
open Clac3.P2.DomainUtil
open Clac3.P2.Function

module Funcs =
    module Ints =
        let add = P2Args.two(fun a b -> P2Args.getInt(a) + P2Args.getInt(b) |> FInteger)
        let sub = P2Args.two(fun a b -> P2Args.getInt(a) - P2Args.getInt(b) |> FInteger)
        let mul = P2Args.two(fun a b -> P2Args.getInt(a) * P2Args.getInt(b) |> FInteger)
        let div = P2Args.two(fun a b -> P2Args.getInt(a) / P2Args.getInt(b) |> FInteger)
        let eq = P2Args.two(fun a b -> P2Args.getInt(a) = P2Args.getInt(b) |> FBool)

    module Floats = 
        let add = P2Args.two(fun a b -> P2Args.getFloat(a) + P2Args.getFloat(b) |> FFloat)
        let sub = P2Args.two(fun a b -> P2Args.getFloat(a) - P2Args.getFloat(b) |> FFloat)
        let mul = P2Args.two(fun a b -> P2Args.getFloat(a) * P2Args.getFloat(b) |> FFloat)
        let div = P2Args.two(fun a b -> P2Args.getFloat(a) / P2Args.getFloat(b) |> FFloat)
        let eq = P2Args.two(fun a b -> P2Args.getFloat(a) = P2Args.getFloat(b) |> FBool)

let numericFuncs = 
    [|
        ("addInts", Funcs.Ints.add)
        ("subInts", Funcs.Ints.sub)
        ("mulInts", Funcs.Ints.mul)
        ("divInts", Funcs.Ints.div)
        ("intEquals", Funcs.Ints.eq)

        ("addFloats", Funcs.Floats.add)
        ("subFloats", Funcs.Floats.sub)
        ("mulFloats", Funcs.Floats.mul)
        ("divFloats", Funcs.Floats.div)
        ("floatEquals", Funcs.Floats.eq)
    |] 
    |> Array.map (fun (name, body) -> { ident = name; lambda = BuiltIn body })

let coreFunctions: S1.FunctionDefinition array = numericFuncs