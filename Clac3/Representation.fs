module Clac3.Representation

open Clac3.Domain

module rec ToString =
    let expressionInner = function
        | Integer i -> string i
        | Float f -> string f
        | Bool b -> string b
        | String s -> "\"" + s + "\""
        | List l -> l |> List.map expression |> String.concat ", " |> inSquareParans
        | Variable r -> "$" + r
        | Keyword k -> k

        | Node children -> children |> node |> inParans

    let expression = function
        | Node children -> node children
        | expr -> expressionInner expr

    let private node = List.map expressionInner >> String.concat " "
    let private inParans s = "(" + s + ")"
    let private inSquareParans s = "[" + s + "]"