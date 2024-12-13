module Clac3.P2.DomainUtil


open Clac3.DomainUtil
open Clac3.FExpression

module rec P2ToString =
    let atom = function
        | FInteger i -> string i
        | FFloat f -> string f
        | FBool b -> string b
        | FString s -> "\"" + s + "\""
        | FArray l -> l |> Array.map atom |> String.concat ", " |> ToString.inSquareParans

    let private expressionInner (bindingRelation: Map<int, string>) = function
        | S2.FExpression.FAtom a -> atom a
        | S2.FExpression.FCall (ident, children) -> 
            children |> node bindingRelation |> prepend bindingRelation[ident] |> ToString.inParans
        | S2.FExpression.FBranch (cond,ifB,elseB) -> 
            S2.FExpression.FCall(-1, [|cond;ifB;elseB|]) |> expressionInner bindingRelation

    let expression (bindingRelationOrig: Map<int, string>) expr =
        let bindingRelation = Map.add -1 "ifthenelse" bindingRelationOrig
        match expr with
        | S2.FExpression.FCall (ident, children) -> node bindingRelation children |> prepend bindingRelation[ident]
        | expr -> expressionInner bindingRelation expr

    let node bindingRelation = Array.map (expressionInner bindingRelation) >> String.concat " "
    let prepend a s = if s = "" then a else sprintf "%s %s" a s

let faBo = FBool >> S1.FExpression.FAtom
let faInt = FInteger >> S1.FExpression.FAtom
let faFl = FFloat >> S1.FExpression.FAtom
let faStr = FString >> S1.FExpression.FAtom
let faLi = FArray >> S1.FExpression.FAtom

module Error =
    let unknownFunction fnCall = 
        failwithf "Unknown function: %A with args %A" (fnCall |> fst) (fnCall |> snd) 

module P2Args = 
    let zero v _ = v
    let one matcher (args: FAtom array) = matcher args[0]
    let two matcher (args: FAtom array) = matcher args[0] args[1]
    let three matcher (args: FAtom array) = matcher args[0] args[1] args[2]
    let four matcher (args: FAtom array) = matcher args[0] args[1] args[2] args[3]
    let five matcher (args: FAtom array) = matcher args[0] args[1] args[2] args[3] args[4]
    let six matcher (args: FAtom array) = matcher args[0] args[1] args[2] args[3] args[4] args[5]
    let seven matcher (args: FAtom array) = matcher args[0] args[1] args[2] args[3] args[4] args[5] args[6]
    let eight matcher (args: FAtom array) = matcher args[0] args[1] args[2] args[3] args[4] args[5] args[6] args[7]
    let nine matcher (args: FAtom array) = matcher args[0] args[1] args[2] args[3] args[4] args[5] args[6] args[7] args[8]
    let ten matcher (args: FAtom array) = matcher args[0] args[1] args[2] args[3] args[4] args[5] args[6] args[7] args[8] args[9]

    let getBool = function
        | FBool b -> b
        | item -> Error.typeError "bool" item
    let getInt = function
        | FInteger i -> i
        | item -> Error.typeError "int" item
    let getFloat = function
        | FFloat f -> f
        | item -> Error.typeError "float" item
    let getString = function
        | FString s -> s
        | item -> Error.typeError "string" item
    let getList = function
        | FArray l -> l
        | item -> Error.typeError "list" item