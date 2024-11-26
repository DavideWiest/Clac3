module Clac3.DomainUtil

open Clac3.Domain

module Program =
    let init rules expressions = {
        rewriteRules = rules
        freeExpressions = expressions;
    }

let isDefined = function
    | Variable _
    | Node _ -> false
    | _ -> true

let matchNode matcher = function
    | Node children -> matcher children
    | _ -> None

let rec toExpression (x: obj) = 
    match x with
    | :? Expression as e -> e
    | :? bool as b -> Bool b
    | :? int as i -> Integer i
    | :? float as f -> Float f
    // strings are always keywords
    | :? string as s -> Keyword s
    | :? list<obj> as l -> List (l |> List.map toExpression)
    | _ -> failwithf "Expected expression-compatible type, got %A" x

let rec private toPattern (x: obj) =
    match x with
    | :? Pattern as p -> p
    | :? LeafPattern as l -> PLeaf l
    | :? bool as b -> PLeaf (PBoolValue b)
    | :? int as i -> PLeaf (PIntegerValue i)
    | :? float as f -> PLeaf (PFloatValue f)
    | :? string as s -> PLeaf (PKeywordValue s)
    | :? list<obj> as l -> PListValue (l |> List.map toPattern)
    | _ -> failwithf "Expected pattern-compatible object, got %A" x

let pAny = PAny
let pNC = PNodeContaining

let pBo = PLeaf PBool
let pInt = PLeaf PInteger
let pFl = PLeaf PFloat
let pStr = PLeaf PString
let pLi = PLeaf PList
let pVar = PLeaf PVariable
let pKw = PLeaf PKeyword

let pN = PLeaf PNode

let vBo = PBoolValue >> PLeaf
let vInt = PIntegerValue >> PLeaf
let vFl = PFloatValue >> PLeaf
let vStr = PStringValue >> PLeaf
let vVar = PVariableValue >> PLeaf
let vKw = PKeywordValue >> PLeaf

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

    let getBool = function
        | Bool b -> b
        | item -> typeError "bool" item

    let getInt = function
        | Integer i -> i
        | item -> typeError "int" item

    let getFloat = function
        | Float f -> f
        | item -> typeError "float" item

    let getString = function
        | String s -> s
        | item -> typeError "string" item

    let getVariable = function
        | Variable v -> v
        | item -> typeError "variable" item

    let getList = function
        | List l -> l
        | item -> typeError "list" item

let private pNodeInner wrapperFn elements =
    elements
    |> Microsoft.FSharp.Reflection.FSharpValue.GetTupleFields
    |> Array.map toPattern
    |> List.ofArray
    |> wrapperFn

let pNode elements = pNodeInner PNodeContaining elements
//let pNodeStartingWith elements = pNodeInner PNodeStartingWith elements

let node elements = 
    elements
    |> Microsoft.FSharp.Reflection.FSharpValue.GetTupleFields
    |> Array.map toExpression
    |> List.ofArray
    |> Node
