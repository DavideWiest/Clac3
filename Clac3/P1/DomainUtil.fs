module Clac3.P1.DomainUtil

open Clac3.Util
open Clac3.Expression
open Clac3.Type
open Clac3.P1.Domain
open Clac3.P1.PatternReplacer

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
        | Array l -> l |> Array.map expression |> String.concat ", " |> ToString.inBrackets
        | Node children -> children |> node |> ToString.inParans

    let expression = function
        | Array l-> l |> Array.map expression |> String.concat ", " |> ToString.inBrackets
        | Node children -> node children
        | expr -> expressionInner expr

    let node = List.map expressionInner >> String.concat " "

let toDefinition (ident, signatureList: Type list, body) : RawBinding = 
    if signatureList.Length = 0 then failwithf "Signature requires at least 1 type. Got: %A" signatureList
    let argTypes, outputType = signatureList[..signatureList.Length-2], signatureList[signatureList.Length-1]

    {
        ident = ident
        signature = { args = argTypes; returnType = outputType }
        body = body
    }

let toRawValue body = RValue body
let toRawCustomFn (argIdents, body) = RCustom { argIdents = Array.ofList argIdents; body = body }


// value patterns
let vAtom = Value >> PAtom

let vBo = Value >> PBool >> vAtom >> Value
let vInt = Value >> PInteger >> vAtom >> Value
let vFl = Value >> PFloat >> vAtom >> Value
let vStr = Value >> PString >> vAtom >> Value
let vVar = Value >> PVariable >> vAtom >> Value
let vKw = Value >> PKeyword >> vAtom >> Value

// any patterns
let pBo = Any |> PBool |> vAtom |> Value
let pInt = Any |> PInteger |> vAtom |> Value
let pFl = Any |> PFloat |> vAtom |> Value
let pStr = Any |> PString |> vAtom |> Value
let pVar = Any |> PVariable |> vAtom |> Value
let pKw = Any |> PKeyword |> vAtom |> Value

let pLi = Any |> PArray |> Value
let pNo = Any |> PNode |> Value

// List/node containing
let LC = (List.map CValue) >> Value >> PArray >> Value
let NC = (List.map CValue) >> Value >> PNode >> Value

// list/node containing with collector
let LCC parts = parts |> List.map CValue |> fun a -> List.append a [CRest] |> Value |> PNode |> Value
let NCC parts = parts |> List.map CValue |> fun a -> List.append a [CRest] |> Value |> PNode |> Value

// expression parts
let aBo = Bool >> Atom
let aInt = Integer >> Atom
let aFl = Float >> Atom
let aStr = String >> Atom
let aVar = Variable >> Atom
let aKw = Keyword >> Atom

let aLi = Array
let aNo = Node

module Args =
    let zero v _ = v
    let one matcher (args: Expression list) : Expression = matcher args[0]
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

    let getChildren = function
        | Node children -> children
        | expr -> Error.typeError "node" expr