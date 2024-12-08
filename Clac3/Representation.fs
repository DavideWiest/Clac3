module Clac3.Representation

open Clac3.Expression

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
        | List l -> l |> List.map expression |> String.concat ", " |> inSquareParans
        | Node children -> children |> node |> inParans

    let expression = function
        | List l-> l |> List.map expression |> String.concat ", " |> inSquareParans
        | Node children -> node children
        | expr -> expressionInner expr

    let private node = List.map expressionInner >> String.concat " "
    let private inParans s = "(" + s + ")"
    let private inSquareParans s = "[" + s + "]"