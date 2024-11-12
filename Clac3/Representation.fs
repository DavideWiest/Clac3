module Clac3.Representation

open Clac3.Domain

module rec ToString =
    let rec symbol = function
        | Integer i -> string i
        | Float f -> string f
        | Bool b -> string b
        | String s -> "\"" + s + "\""
        | List l -> l |> List.map expression |> String.concat ", " |> fun s -> "[" + s + "]"
        | Variable r -> "$" + r
        | Keyword k -> k

    let rec expressionInner = function
        | Leaf s -> symbol s
        | Node children -> children |> List.map expressionInner |> String.concat " " |> fun s -> "(" + s + ")"

    let rec expression = function
        | Leaf s -> symbol s
        | Node children -> children |> List.map expressionInner |> String.concat " "