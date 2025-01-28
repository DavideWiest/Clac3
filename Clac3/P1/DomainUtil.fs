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
        | Array l -> l |> List.map expression |> String.concat ", " |> ToString.inBrackets
        | Node children -> children |> node |> ToString.inParans

    let expression = function
        | Array l-> l |> List.map expression |> String.concat ", " |> ToString.inBrackets
        | Node children -> node children
        | expr -> expressionInner expr

    let node = List.map expressionInner >> String.concat " "


module Interpreter = 
    let rec valueReplacerWrapper tryReplaceValue exprValue = 
        match tryReplaceValue exprValue with
        | Some exprValue' -> valueReplacerWrapper tryReplaceValue exprValue'
        | None -> exprValue

    // must be lifted every time it's replaced?
    let rec replacerWrapper lift tryReplace expr : TAExpression = 
        match tryReplace expr with
        | Some expr' -> replacerWrapper lift tryReplace (lift expr')
        | None -> expr

    let compareType expected taExpr = 
        if expected <> None && expected <> Some taExpr.eType then Error.typeError expected taExpr.eType taExpr.expr

        taExpr


module Lift = 
    let typeAnnotateAtom (bindingTypeMap: Map<string, Type>) = function
        | Bool _ -> TBool
        | Integer _ -> TInteger
        | Float _ -> TFloat
        | String _ -> TString
        | Variable v -> bindingTypeMap[v]
        | Keyword _ -> TKeyword

    let liftAtom bindingTypeMap a = { expr=TAAtom a; eType=typeAnnotateAtom bindingTypeMap a }

    let typeAnnotateAtomWithoutBindings = function
        | Bool _ -> TBool
        | Integer _ -> TInteger
        | Float _ -> TFloat
        | String _ -> TString
        | Variable v -> failwith "Calling typeAnnotateAtomWithoutBindings with a variable is not allowed."
        | Keyword _ -> TKeyword

    let liftAtomWithoutBindings a = { expr=TAAtom a; eType=typeAnnotateAtomWithoutBindings a }

let toDefinition (ident, signatureList: Type list, body) : RawBinding = 
    if signatureList.Length = 0 then failwith "Signature requires at least 1 type. Got an empty signature."
    let argTypes, outputType = signatureList[..signatureList.Length-2], signatureList[signatureList.Length-1]

    {
        ident = ident
        signature = { args = argTypes; returnType = outputType }
        body = body
    }

let toRawValue body = RValue body
let toRawCustomFn (argIdents, body) = RCustom { argIdents = Array.ofList argIdents; body = body }


// value patterns
let vpAtom = Value >> PAtom >> Value

let vBo = Value >> PBool >> vpAtom
let vInt = Value >> PInteger >> vpAtom
let vFl = Value >> PFloat >> vpAtom
let vStr = Value >> PString >> vpAtom
let vVar = Value >> PVariable >> vpAtom
let vKw = Value >> PKeyword >> vpAtom

// any patterns
let pBo = Any |> PBool |> vpAtom
let pInt = Any |> PInteger |> vpAtom
let pFl = Any |> PFloat |> vpAtom
let pStr = Any |> PString |> vpAtom
let pVar = Any |> PVariable |> vpAtom
let pKw = Any |> PKeyword |> vpAtom

let pLi = SimplePatternUnion.Any |> PArray |> Value
let pNo = SimplePatternUnion.Any |> PNode |> Value

// List/node containing
let LC = (List.map CValue) >> SimplePatternUnion.Value >> PArray >> Value
let NC = (List.map CValue) >> SimplePatternUnion.Value >> PNode >> Value

// list/node containing with collector
let LCC parts = parts |> List.map CValue |> fun a -> List.append a [CRest] |> SimplePatternUnion.Value |> PNode |> Value
let NCC parts = parts |> List.map CValue |> fun a -> List.append a [CRest] |> SimplePatternUnion.Value |> PNode |> Value

// expression parts
let aBo = Bool >> Atom
let aInt = Integer >> Atom
let aFl = Float >> Atom
let aStr = String >> Atom
let aVar = Variable >> Atom
let aKw = Keyword >> Atom

let aLi = Array
let aNo = Node

// type-annotated expression parts
let taBo = Bool >> Lift.liftAtomWithoutBindings
let taInt = Integer >> Lift.liftAtomWithoutBindings
let taFl = Float >> Lift.liftAtomWithoutBindings
let taStr = String >> Lift.liftAtomWithoutBindings
let taVar = Variable >> Lift.liftAtomWithoutBindings
let taKw = Keyword >> Lift.liftAtomWithoutBindings

let taLi = TAArray
let taNo = TANode


module Args =
    let zero v _ = v
    let one matcher (args: TAExpression list) = matcher args[0]
    let two matcher (args: TAExpression list) = matcher args[0] args[1]
    let three matcher (args: TAExpression list) = matcher args[0] args[1] args[2]
    let four matcher (args: TAExpression list) = matcher args[0] args[1] args[2] args[3]
    let five matcher (args: TAExpression list) = matcher args[0] args[1] args[2] args[3] args[4]
    let six matcher (args: TAExpression list) = matcher args[0] args[1] args[2] args[3] args[4] args[5]
    let seven matcher (args: TAExpression list) = matcher args[0] args[1] args[2] args[3] args[4] args[5] args[6]
    let eight matcher (args: TAExpression list) = matcher args[0] args[1] args[2] args[3] args[4] args[5] args[6] args[7]
    let nine matcher (args: TAExpression list) = matcher args[0] args[1] args[2] args[3] args[4] args[5] args[6] args[7] args[8]
    let ten matcher (args: TAExpression list) = matcher args[0] args[1] args[2] args[3] args[4] args[5] args[6] args[7] args[8] args[9]

    let getChildren = function
        | TANode children -> children
        | expr -> Error.typeError "node" expr