module Clac3.P2.BuiltIn

open Clac3.TypeAnnotatedExpression
open Clac3.FExpression
open Clac3.Function
open Clac3.P2.DomainUtil

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

let intFuncs = 
    [|
        ("addInts", Funcs.Ints.add)
        ("subInts", Funcs.Ints.sub)
        ("mulInts", Funcs.Ints.mul)
        ("divInts", Funcs.Ints.div)
    |]
    |> Array.map (fun (name, body) -> { ident = name; lambda = BuiltIn body; signature = [TInteger; TInteger], TInteger })

let floatFuncs = 
    [|
        ("addFloats", Funcs.Floats.add)
        ("subFloats", Funcs.Floats.sub)
        ("mulFloats", Funcs.Floats.mul)
        ("divFloats", Funcs.Floats.div)
    |]
    |> Array.map (fun (name, body) -> { ident = name; lambda = BuiltIn body; signature = [TFloat; TFloat], TFloat })

let equalityFuncs = 
    [|
        ("intEquals", Funcs.Ints.eq, TInteger)
        ("floatEquals", Funcs.Floats.eq, TFloat)
    |]
    |> Array.map (fun (name, body, returnType) -> { ident = name; lambda = BuiltIn body; signature = [returnType; returnType], TBool })
    
let coreFunctions: S1.FunctionDefinition array = Array.concat [intFuncs; floatFuncs; equalityFuncs]