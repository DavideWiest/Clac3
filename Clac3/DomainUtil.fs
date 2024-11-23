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
    | :? ExpressionType as et -> Get et
    | :? Expression as e -> Value e
    | _ -> try Value (toExpression x) with | Failure(_) -> failwithf "Expected pattern-compatible object, got %A" x

module Args =
    let zero v (args: 'a list) = toExpression v
    let one matcher (args: 'a list) = matcher args[0] |> toExpression
    let two matcher (args: 'a list) = matcher args[0] args[1] |> toExpression
    let three matcher (args: 'a list) = matcher args[0] args[1] args[2] |> toExpression
    let four matcher (args: 'a list) = matcher args[0] args[1] args[2] args[3] |> toExpression
    let five matcher (args: 'a list) = matcher args[0] args[1] args[2] args[3] args[4] |> toExpression
    let six matcher (args: 'a list) = matcher args[0] args[1] args[2] args[3] args[4] args[5] |> toExpression
    let seven matcher (args: 'a list) = matcher args[0] args[1] args[2] args[3] args[4] args[5] args[6] |> toExpression

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

let private tnodeInner wrapperFn elements =
    elements
    |> Microsoft.FSharp.Reflection.FSharpValue.GetTupleFields
    |> Array.map toPattern
    |> List.ofArray
    |> wrapperFn

let tnode elements = tnodeInner TNodeContaining elements
let tnodeStartingWith elements = tnodeInner TNodeStartingWith elements

let GetNode t = t |> tnode |> Get
let GetNodeStartingWith t = t |> tnodeStartingWith |> Get

let private nodeInner exprFunc elements = 
    elements
    |> Microsoft.FSharp.Reflection.FSharpValue.GetTupleFields
    |> Array.map exprFunc
    |> List.ofArray
    |> Node

let nodeAsPattern elements = nodeInner toExpression elements
let node elements = nodeInner toExpression elements

let ValueNode t = t |> nodeAsPattern |> Value