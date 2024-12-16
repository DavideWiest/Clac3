module Clac3.P2.DomainUtil

open Clac3.Expression
open Clac3.DomainUtil
open Clac3.FunctionalExpression

module rec P2ToString =
    let atom = function
        | Integer i -> string i
        | Float f -> string f
        | Bool b -> string b
        | String s -> "\"" + s + "\""

    let private expressionInner (bindingRelation: Map<int, string>) = function
        | S2.FExpression.FAtom a -> atom a
        | S2.FExpression.FArray a -> a |> node bindingRelation |> ToString.inBrackets
        | S2.FExpression.FCall { ident=ident; args=args } -> 
            args |> node bindingRelation |> prepend bindingRelation[ident] |> ToString.inParans
        | S2.FExpression.FBranch { cond = cond; trueB = trueB; falseB = falseB } -> 
            { ident=(-1); args=[|cond;trueB;falseB|] } |> S2.FExpression.FCall |> expressionInner bindingRelation

    let expression (bindingRelationOrig: Map<int, string>) expr =
        let bindingRelation = Map.add -1 "ifthenelse" bindingRelationOrig
        match expr with
        | S2.FExpression.FCall { ident=ident; args=args } -> node bindingRelation args |> prepend bindingRelation[ident]
        | expr -> expressionInner bindingRelation expr

    let node bindingRelation = Array.map (expressionInner bindingRelation) >> String.concat " "
    let prepend a s = if s = "" then a else sprintf "%s %s" a s

let toFCall (ident, args) = S1.FExpression.FCall { ident = ident; args = args }
let toBranch (cond, trueB, falseB) = S1.FExpression.FBranch { cond = cond; trueB = trueB; falseB = falseB }

let faBo = Bool >> S1.FExpression.FAtom
let faInt = Integer >> S1.FExpression.FAtom
let faFl = Float >> S1.FExpression.FAtom
let faStr = String >> S1.FExpression.FAtom

module Error =
    let unknownFunction fnCall = 
        failwithf "Unknown function: %A with args %A" (fnCall |> fst) (fnCall |> snd) 

module P2Args = 
    let zero v _ = v
    let one matcher (args: S2.FExpression array) = matcher args[0]
    let two matcher (args: S2.FExpression array) = matcher args[0] args[1]
    let three matcher (args: S2.FExpression array) = matcher args[0] args[1] args[2]
    let four matcher (args: S2.FExpression array) = matcher args[0] args[1] args[2] args[3]
    let five matcher (args: S2.FExpression array) = matcher args[0] args[1] args[2] args[3] args[4]
    let six matcher (args: S2.FExpression array) = matcher args[0] args[1] args[2] args[3] args[4] args[5]
    let seven matcher (args: S2.FExpression array) = matcher args[0] args[1] args[2] args[3] args[4] args[5] args[6]
    let eight matcher (args: S2.FExpression array) = matcher args[0] args[1] args[2] args[3] args[4] args[5] args[6] args[7]
    let nine matcher (args: S2.FExpression array) = matcher args[0] args[1] args[2] args[3] args[4] args[5] args[6] args[7] args[8]
    let ten matcher (args: S2.FExpression array) = matcher args[0] args[1] args[2] args[3] args[4] args[5] args[6] args[7] args[8] args[9]

    let getBool = function
        | FAtom (Bool b) -> b
        | item -> Error.typeError "bool" item

    let getInt = function
        | FAtom (Integer i) -> i
        | item -> Error.typeError "int" item

    let getFloat = function
        | FAtom (Float f) -> f
        | item -> Error.typeError "float" item

    let getString = function
        | FAtom (String s) -> s
        | item -> Error.typeError "string" item

    let getArray = function
        | Array l -> l
        | item -> Error.typeError "list" item