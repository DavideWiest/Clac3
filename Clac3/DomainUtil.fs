module Clac3.DomainUtil

open Clac3.Domain
open Clac3.Representation

let vAtom = Value >> PDefined

let vBo = Value >> PBool >> vAtom >> Value
let vInt = Value >> PInteger >> vAtom >> Value
let vFl = Value >> PFloat >> vAtom >> Value
let vStr = Value >> PString >> vAtom >> Value

let pBo = Any >> PBool >> vAtom >> Value
let pInt = Any >> PInteger >> vAtom >> Value
let pFl = Any >> PFloat >> vAtom >> Value
let pStr = Any >> PString >> vAtom >> Value
 
let pLi = Any >> PList |> Value
let pNo = Any >> PNode |> Value

let vLi = Value >> PList >> Value
let pNC = Value >> PNode >> Value

let aBo = Bool >> Defined
let aInt = Integer >> Defined
let aFl = Float >> Defined
let aStr = String >> Defined
let aVar = Variable >> Symbol
let aKw = Keyword >> Symbol

let aLi = List
let aNo = Node