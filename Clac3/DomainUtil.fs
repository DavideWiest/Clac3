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
    | :? bool as b -> PBoolValue b
    | :? int as i -> PIntegerValue i
    | :? float as f -> PFloatValue f
    | :? string as s -> PKeywordValue s
    | :? list<obj> as l -> PListValue (l |> List.map toPattern)
    | _ -> failwithf "Expected pattern-compatible object, got %A" x

module Args =
    let zero v (args: 'a list) = toExpression v
    let one matcher (args: 'a list) = matcher args[0] |> toExpression
    let two matcher (args: 'a list) = matcher args[0] args[1] |> toExpression
    let three matcher (args: 'a list) = matcher args[0] args[1] args[2] |> toExpression
    let four matcher (args: 'a list) = matcher args[0] args[1] args[2] args[3] |> toExpression
    let five matcher (args: 'a list) = matcher args[0] args[1] args[2] args[3] args[4] |> toExpression
    let six matcher (args: 'a list) = matcher args[0] args[1] args[2] args[3] args[4] args[5] |> toExpression
    let seven matcher (args: 'a list) = matcher args[0] args[1] args[2] args[3] args[4] args[5] args[6] |> toExpression
    let eight matcher (args: 'a list) = matcher args[0] args[1] args[2] args[3] args[4] args[5] args[6] args[7] |> toExpression
    let nine matcher (args: 'a list) = matcher args[0] args[1] args[2] args[3] args[4] args[5] args[6] args[7] args[8] |> toExpression
    let ten matcher (args: 'a list) = matcher args[0] args[1] args[2] args[3] args[4] args[5] args[6] args[7] args[8] args[9] |> toExpression

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
