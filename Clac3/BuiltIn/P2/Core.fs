module Clac3.BuiltIn.P2.Core

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
        let bools = P2Args.two(fun a b -> P2Args.getBool(a) = P2Args.getBool(b))
        let ints = P2Args.two(fun a b -> P2Args.getInt(a) = P2Args.getInt(b))
        let floats = P2Args.two(fun a b -> P2Args.getFloat(a) = P2Args.getFloat(b))

    module Comp = 
        let ltInt = P2Args.two(fun a b -> P2Args.getInt(a) < P2Args.getInt(b))
        let gtInt = P2Args.two(fun a b -> P2Args.getInt(a) > P2Args.getInt(b))
        let ltFloat = P2Args.two(fun a b -> P2Args.getFloat(a) < P2Args.getFloat(b))
        let gtFloat = P2Args.two(fun a b -> P2Args.getFloat(a) > P2Args.getFloat(b))

    module Boolean = 
        let and_ = P2Args.two(fun a b -> P2Args.getBool(a) && P2Args.getBool(b))
        let or_ = P2Args.two(fun a b -> P2Args.getBool(a) || P2Args.getBool(b))
        let not_ = P2Args.one(fun a -> not (P2Args.getBool(a)))

let intFuncs = 
    [|
        ("addInt", Funcs.Ints.add)
        ("subtractInt", Funcs.Ints.sub)
        ("multiplyInt", Funcs.Ints.mul)
        ("divideInt", Funcs.Ints.div)
    |]
    |> Array.map (fun (name, body) -> { ident = name; binding = BuiltIn (body >> FInteger >> FAtom); signature = { args = [TInteger; TInteger]; returnType=TInteger } })

let floatFuncs = 
    [|
        ("addFloat", Funcs.Floats.add)
        ("subtractFloat", Funcs.Floats.sub)
        ("multiplyFloat", Funcs.Floats.mul)
        ("divideFloat", Funcs.Floats.div)
    |]
    |> Array.map (fun (name, body) -> { ident = name; binding = BuiltIn (body >> FFloat >> FAtom); signature = { args = [TFloat; TFloat]; returnType=TFloat } })

let equalityFuncs = 
    [|
        ("eqBool", Funcs.Eq.bools, TBool)
        ("eqInt", Funcs.Eq.ints, TInteger)
        ("eqFloat", Funcs.Eq.floats, TFloat)
    |]
    |> Array.map (fun (name, body, argType) -> { ident = name; binding = BuiltIn (body >> FBool >> FAtom); signature = { args = [argType; argType]; returnType=TBool } })

let compFuncs = 
    [|
        ("ltInt", Funcs.Comp.ltInt, TBool)
        ("gtInt", Funcs.Comp.gtInt, TBool)
        ("ltFloat", Funcs.Comp.ltFloat, TBool)
        ("gtFloat", Funcs.Comp.gtFloat, TBool)
    |]
    |> Array.map (fun (name, body, argType) -> { ident = name; binding = BuiltIn (body >> FBool >> FAtom); signature = { args = [argType; argType]; returnType=TBool } })

let boolFuncs = 
    [|
        ("and", Funcs.Boolean.and_, TBool)
        ("or", Funcs.Boolean.or_, TBool)
        ("not", Funcs.Boolean.not_, TBool)
    |]
    |> Array.map (fun (name, body, argType) -> { ident = name; binding = BuiltIn (body >> FBool >> FAtom); signature = { args = [argType; argType]; returnType=TBool } })
    
let coreBindings: S1.Binding array = Array.concat [intFuncs; floatFuncs; equalityFuncs; compFuncs; boolFuncs]