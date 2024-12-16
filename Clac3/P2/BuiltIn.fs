module Clac3.P2.BuiltIn

open Clac3.Expression
open Clac3.Type
open Clac3.FunctionalExpression
open Clac3.Function
open Clac3.P2.DomainUtil

module Funcs =
    module Ints =
        let add = P2Args.two(fun a b -> P2Args.getInt(a) + P2Args.getInt(b))
        let sub = P2Args.two(fun a b -> P2Args.getInt(a) - P2Args.getInt(b))
        let mul = P2Args.two(fun a b -> P2Args.getInt(a) * P2Args.getInt(b))
        let div = P2Args.two(fun a b -> P2Args.getInt(a) / P2Args.getInt(b))

    module Floats = 
        let add = P2Args.two(fun a b -> P2Args.getFloat(a) + P2Args.getFloat(b))
        let sub = P2Args.two(fun a b -> P2Args.getFloat(a) - P2Args.getFloat(b))
        let mul = P2Args.two(fun a b -> P2Args.getFloat(a) * P2Args.getFloat(b))
        let div = P2Args.two(fun a b -> P2Args.getFloat(a) / P2Args.getFloat(b))

    module Eq =
        let ints = P2Args.two(fun a b -> P2Args.getInt(a) = P2Args.getInt(b))
        let floats = P2Args.two(fun a b -> P2Args.getFloat(a) = P2Args.getFloat(b))

let intFuncs = 
    [|
        ("addInts", Funcs.Ints.add)
        ("subInts", Funcs.Ints.sub)
        ("mulInts", Funcs.Ints.mul)
        ("divInts", Funcs.Ints.div)
    |]
    |> Array.map (fun (name, body) -> { ident = name; binding = BuiltIn (body >> Integer >> FAtom); signature = { args = [TInteger; TInteger]; returnType=TInteger } })

let floatFuncs = 
    [|
        ("addFloats", Funcs.Floats.add)
        ("subFloats", Funcs.Floats.sub)
        ("mulFloats", Funcs.Floats.mul)
        ("divFloats", Funcs.Floats.div)
    |]
    |> Array.map (fun (name, body) -> { ident = name; binding = BuiltIn (body >> Float >> FAtom); signature = { args = [TFloat; TFloat]; returnType=TFloat } })

let equalityFuncs = 
    [|
        ("intEquals", Funcs.Eq.ints, TInteger)
        ("floatEquals", Funcs.Eq.floats, TFloat)
    |]
    |> Array.map (fun (name, body, argType) -> { ident = name; binding = BuiltIn (body >> Bool >> FAtom); signature = { args = [argType; argType]; returnType=TBool } })
    
let coreFunctions: S1.Binding array = Array.concat [intFuncs; floatFuncs; equalityFuncs]