module Clac3.P2.BuiltIn

open Clac3.P2.FExpression
open Clac3.P2.DomainUtil
open Clac3.P2.Function

module Funcs =
    let addInts = P2Args.two(fun a b -> P2Args.getInt(a) + P2Args.getInt(b) |> FInteger)
    let subInts = P2Args.two(fun a b -> P2Args.getInt(a) - P2Args.getInt(b) |> FInteger)
    let mulInts = P2Args.two(fun a b -> P2Args.getInt(a) * P2Args.getInt(b) |> FInteger)
    let divInts = P2Args.two(fun a b -> P2Args.getInt(a) / P2Args.getInt(b) |> FInteger)

    let intEquals = P2Args.two(fun a b -> P2Args.getInt(a) = P2Args.getInt(b) |> FBool)

let arithmeticFunctions = 
    [|
        ("addInts", Funcs.addInts)
        ("subInts", Funcs.subInts)
        ("mulInts", Funcs.mulInts)
        ("divInts", Funcs.divInts)
        ("intEquals", Funcs.intEquals)
    |] 
    |> Array.map (fun (name, body) -> { ident = name; lambda = BuiltIn body })

let coreFunctions: S1.FunctionDefinition array = arithmeticFunctions