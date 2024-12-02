﻿module Clac3.DomainUtil

open Clac3.Domain

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

    let private typeError typeString item = failwithf "Expected %s, got %A" typeString item

    let getNode = function
        | Node children -> children
        | item -> typeError "node" item

    let getList = function
        | List l -> l
        | item -> typeError "list" item

    let getBool = function
        | Atom (Bool b) -> b
        | item -> typeError "bool" item

    let getInt = function
        | Atom (Integer i) -> i
        | item -> typeError "int" item

    let getFloat = function
        | Atom (Float f) -> f
        | item -> typeError "float" item

    let getString = function
        | Atom (String s) -> s
        | item -> typeError "string" item

    let getVariable = function
        | Atom (Variable v) -> v
        | item -> typeError "variable" item

    let getKeyword = function
        | Atom (Keyword kw) -> kw
        | item -> typeError "keyword" item