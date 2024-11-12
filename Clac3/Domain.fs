module Clac3.Domain

type Symbol =
    | Bool of bool 
    | Integer of int
    | Float of float
    | String of string
    | List of Expression list // this should probably be Expression instead
    | Variable of string
    | Keyword of string // only predefined symbols

and Expression =
    | Leaf of Symbol
    | Node of Expression list

type RewriteRuleSet = {
    pattern: Expression -> Expression option;
}

type Program = {
    rewriteRules: RewriteRuleSet list;
    freeExpressions: Expression list;
}

let SNode = Node >> Some
let SLeaf = Leaf >> Some

let BoolLeaf = Bool >> SLeaf
let IntLeaf = Integer >> SLeaf
let FloatLeaf = Float >> SLeaf
let StringLeaf = String >> SLeaf
let ListLeaf = List >> SLeaf

let BoolLeafPrepended x (rest: Expression list) = if rest.Length = 0 then BoolLeaf x else (Leaf (Bool x))::rest |> Node |> Some
let IntLeafPrepended x (rest: Expression list) = if rest.Length = 0 then IntLeaf x else (Leaf (Integer x))::rest |> Node |> Some
let FloatLeafPrepended x (rest: Expression list) = if rest.Length = 0 then FloatLeaf x else (Leaf (Float x))::rest |> Node |> Some
let StringLeafPrepended x (rest: Expression list) = if rest.Length = 0 then StringLeaf x else (Leaf (String x))::rest |> Node |> Some
let ListLeafPrepended x (rest: Expression list) = if rest.Length = 0 then ListLeaf x else (Leaf (List x))::rest |> Node |> Some

let isDefinedLeaf = function
    | Variable _ -> false
    | _ -> true

let exprIsDefinedLeaf = function
        | Leaf l -> isDefinedLeaf l
        | _ -> false

module Program =
    let init rules expressions = {
        rewriteRules = rules
        freeExpressions = expressions;
    }

let matchNode matcher = function
    | Node children -> matcher children
    | _ -> None