module Clac3.DomainUtil

open Clac3.Domain

module Program =
    let init rules expressions = {
        rewriteRules = rules
        freeExpressions = expressions;
    }

let vLeaf = Value >> PLeaf

let vBo = Value >> PBool >> vLeaf
let vInt = Value >> PInteger >> vLeaf
let vFl = Value >> PFloat >> vLeaf
let vStr = Value >> PString >> vLeaf
let vVar = Value >> PVariable >> vLeaf
let vKw = Value >> PKeyword >> vLeaf

let pBo = Any |> PBool |> vLeaf
let pInt = Any |> PInteger |> vLeaf
let pFl = Any |> PFloat |> vLeaf
let pStr = Any |> PString |> vLeaf
let pVar = Any |> PVariable |> vLeaf
let pKw = Any |> PKeyword |> vLeaf

let pLi = Any |> PList
let pNo = Any |> PNode

let vLi = Value >> PList
let pNC = Value >> PNode

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
