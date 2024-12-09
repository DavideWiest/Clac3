﻿module Clac3.P1.DomainUtil

open Clac3.DomainUtil
open Clac3.P1.Expression
open Clac3.P1.Domain

module rec ToString =
    let atom = function
        | Integer i -> string i
        | Float f -> string f
        | Bool b -> string b
        | String s -> "\"" + s + "\""
        | Variable r -> "$" + r
        | Keyword k -> k

    let expressionInner = function
        | Atom a -> atom a
        | List l -> l |> List.map expression |> String.concat ", " |> ToString.inSquareParans
        | Node children -> children |> node |> ToString.inParans

    let expression = function
        | List l-> l |> List.map expression |> String.concat ", " |> ToString.inSquareParans
        | Node children -> node children
        | expr -> expressionInner expr

    let node = List.map expressionInner >> String.concat " "

let vAtom = Value >> PAtom

let vBo = Value >> PBool >> vAtom >> Value
let vInt = Value >> PInteger >> vAtom >> Value
let vFl = Value >> PFloat >> vAtom >> Value
let vStr = Value >> PString >> vAtom >> Value
let vVar = Value >> PVariable >> vAtom >> Value
let vKw = Value >> PKeyword >> vAtom >> Value

let pBo = Any |> PBool |> vAtom |> Value
let pInt = Any |> PInteger |> vAtom |> Value
let pFl = Any |> PFloat |> vAtom |> Value
let pStr = Any |> PString |> vAtom |> Value
let pVar = Any |> PVariable |> vAtom |> Value
let pKw = Any |> PKeyword |> vAtom |> Value

let pLi = Any |> PList |> Value
let pNo = Any |> PNode |> Value

let vLi = Value >> PList >> Value
let pNC = Value >> PNode >> Value

let aBo = Bool >> Atom
let aInt = Integer >> Atom
let aFl = Float >> Atom
let aStr = String >> Atom
let aVar = Variable >> Atom
let aKw = Keyword >> Atom

let aLi = List
let aNo = Node

module Args =
    let zero v _ = v
    let one matcher (args: Expression list) = matcher args[0]
    let two matcher (args: Expression list) = matcher args[0] args[1]
    let three matcher (args: Expression list) = matcher args[0] args[1] args[2]
    let four matcher (args: Expression list) = matcher args[0] args[1] args[2] args[3]
    let five matcher (args: Expression list) = matcher args[0] args[1] args[2] args[3] args[4]
    let six matcher (args: Expression list) = matcher args[0] args[1] args[2] args[3] args[4] args[5]
    let seven matcher (args: Expression list) = matcher args[0] args[1] args[2] args[3] args[4] args[5] args[6]
    let eight matcher (args: Expression list) = matcher args[0] args[1] args[2] args[3] args[4] args[5] args[6] args[7]
    let nine matcher (args: Expression list) = matcher args[0] args[1] args[2] args[3] args[4] args[5] args[6] args[7] args[8]
    let ten matcher (args: Expression list) = matcher args[0] args[1] args[2] args[3] args[4] args[5] args[6] args[7] args[8] args[9]

    let printAndPass (args: Expression list) = 
        printfn "args: %A" (args |> List.map ToString.expression)
        args

    let getNode = function
        | Node children -> children
        | item -> Error.typeError "node" item
    let getList = function
        | List l -> l
        | item -> Error.typeError "list" item
    let getBool = function
        | Atom (Bool b) -> b
        | item -> Error.typeError "bool" item
    let getInt = function
        | Atom (Integer i) -> i
        | item -> Error.typeError "int" item
    let getFloat = function
        | Atom (Float f) -> f
        | item -> Error.typeError "float" item
    let getString = function
        | Atom (String s) -> s
        | item -> Error.typeError "string" item
    let getVariable = function
        | Atom (Variable v) -> v
        | item -> Error.typeError "variable" item
    let getKeyword = function
        | Atom (Keyword kw) -> kw
        | item -> Error.typeError "keyword" item
