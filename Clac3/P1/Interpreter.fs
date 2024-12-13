module Clac3.P1.SubstitutionInterpreter

open Clac3.Expression
open Clac3.TypeAnnotatedExpression
open Clac3.FExpression
open Clac3.Function

let rec handleListLikeType wrapperFn tryReplace defaultFn children =
    let children' = children |> List.map (evalExpr tryReplace defaultFn)

    children' |> wrapperFn |> tryReplace |> Option.map (evalExpr tryReplace defaultFn)
    |> fun r -> if r.IsSome then r.Value else children' |> wrapperFn |> defaultFn
    
and evalExpr (tryReplace: Expression -> Expression option) (defaultFn: Expression -> Expression) = function
    | Atom a -> tryReplace (Atom a) |> Option.defaultValue (defaultFn (Atom a))
    | List children -> handleListLikeType List tryReplace defaultFn children
    | Node children -> handleListLikeType Node tryReplace defaultFn children

let rec getValueType a = 
    match a with
    | Bool _ -> TAAtom (TBool, a)
    | Integer _ -> TAAtom (TInteger, a)
    | Float _ -> TAAtom (TFloat, a)
    | String _ -> TAAtom (TString, a)
    | Variable _ -> TAAtom (TVariable, a)
    | Keyword _ -> TAAtom (TKeyword, a)

let rec getValueTypeForFAtom = function
    | FBool _ -> TBool
    | FInteger _ -> TInteger
    | FFloat _ -> TFloat
    | FString _ -> TString
    | FArray arr -> TList (getValueTypeForFAtom arr[0])

let rec extractType = function
    | TAAtom (t, _) -> t
    | TAList (t, _) -> TList t
    | TANode (t, _) -> t

let rec toTAExprInner (bindingStore: S1.BindingStore) = function
    | Atom a -> getValueType a
    | List children -> TAList (TList TBool, children |> List.map (toTAExprInner bindingStore))
    | Node (children) -> 
        match children with
        // might support this later, but it wouldn't be useful since Clac is lazily evaluated
        | [] -> failwith "Empty node"
        | head::tail -> 
            match head with
            | Node children -> toTAExprInner bindingStore (Node (children@tail)) // flatten the node
            | Atom (Keyword fnName) -> 
                let args = tail |> List.map (toTAExprInner bindingStore)

                match bindingStore.TryFind fnName with
                | None -> failwithf "Function %A not found" fnName
                | Some (BValue v) ->
                    if tail.Length > 0 then failwithf "Value %A does not accept arguments" fnName

                    TANode (getValueTypeForFAtom v, (fnName, args))
                | Some (BFuncDef fn) -> 
                    let argTypes = args |> List.map extractType

                    List.zip (fn.signature |> fst).[..argTypes.Length - 2] argTypes 
                    |> List.iter (fun (tExpected, tActual) -> if not (tExpected = tActual) then failwithf "Expected type %A, got %A when calling %A" tExpected tActual fnName)

                    if fn.signature |> fst |> List.length > argTypes.Length then failwithf "Got too many args for %A: Expected %A, got %A" fnName (fn.signature |> fst |> List.length) argTypes.Length
                    if fn.signature |> fst |> List.length < argTypes.Length then failwithf "Currying not supported yet."

                    let returnType = fn.signature |> snd
                    TANode (returnType, (fnName, args))
            | _ -> failwith "Evaluated node in phase 1 must be convertible to functional form"
        
let toTAExpr functionStore tryReplace defaultFn = evalExpr tryReplace defaultFn >> toTAExprInner functionStore