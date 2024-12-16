module Clac3.P1.SubstitutionInterpreter

open Clac3.Expression
open Clac3.Type
open Clac3.FunctionalExpression
open Clac3.Function

let rec typeAnnotateAtom a = 
    match a with
    | Bool _ -> TBool
    | Integer _ -> TInteger
    | Float _ -> TFloat
    | String _ -> TString
    | Variable _ -> TVariable
    | Keyword _ -> TKeyword

let rec inferAndValidateType (bindingStore: S1.BindingStore) (children: TAExpression list) : Type =
    match children with
    | [] -> Unit
    | head::args ->
        match head with
        | { eType=TFunc _; expr=_ } -> failwith "todo"
        | { eType=TKeyword; expr=Atom (Keyword fnName) } ->
            match bindingStore.TryFind fnName with
            | None -> failwithf "Function %A not found" fnName
            | Some (binding) ->
                let argTypes = args |> List.map (fun a -> a.eType)

                List.zip binding.signature.args.[..argTypes.Length - 2] argTypes 
                |> List.iter (fun (tExpected, tActual) -> 
                    if tExpected <> tActual then failwithf "Expected type %A, got %A when calling %A" tExpected tActual fnName
                )

                if binding.signature.args.Length > argTypes.Length 
                then failwithf "Got too many args for %A: Expected %A, got %A" fnName binding.signature.args.Length argTypes.Length

                let argTypesRemaining = binding.signature.args[args.Length..]

                if argTypesRemaining.Length = 0 
                then binding.signature.returnType 
                else TFunc { args=argTypesRemaining; returnType=binding.signature.returnType }

        | _ -> failwithf "Evaluated node in phase 1 must be convertible to functional form. Expected function or keyword, got: %A" head

let rec evalExprInner (bindingStore: S1.BindingStore) (tryReplace: Expression -> Expression option) expr : TAExpression =
    match expr with
    | Atom a -> tryReplace (Atom a) |> Option.map (evalExprInner bindingStore tryReplace) |> Option.defaultValue ({ expr=Atom a; eType=typeAnnotateAtom a })
    | Array arr -> 
        let arr' = arr |> Array.map (evalExprInner bindingStore tryReplace)

        let types = arr' |> Array.map (fun a -> a.eType)
        let items = arr' |> Array.map (fun a -> a.expr)

        if (types |> Array.distinct |> Array.length) > 1 then failwithf "Array elements must have the same type. Array: %A" arr'

        { expr=Array items; eType=TArray arr'[0].eType }
    | Node children ->
        let children' = children |> List.map (evalExprInner bindingStore tryReplace)
        let childrenWithoutTypes = children' |> List.map (fun c -> c.expr)
        let result = 
            childrenWithoutTypes
            |> Node
            |> tryReplace 
            |> Option.map (evalExprInner bindingStore tryReplace)
        
        if result.IsSome 
        then result.Value 
        else { expr=Node childrenWithoutTypes; eType=inferAndValidateType bindingStore children'}
            
let rec toFunctionalExpression = function
    | Atom a -> FAtom a
    | Array arr -> FArray (arr |> Array.map toFunctionalExpression)
    | Node children -> 
        match children with
        | [] -> failwith "Empty expression list"
        | head::tail ->
            match head with
            | Atom (Keyword fnName) -> FCall { ident=fnName; args=tail |> List.map toFunctionalExpression |> Array.ofList }
            | _ -> failwithf "Expected keyword, got: %A. Full expression: %A" head (Node children)

let toFunctionalExpressionForTopLevel (expectedOutputType: Type option) (taExpr: TAExpression) = 
    if expectedOutputType.IsSome && taExpr.eType <> expectedOutputType.Value then failwithf "Expected type %A, got %A" expectedOutputType taExpr.eType
    toFunctionalExpression taExpr.expr

let evalExpr tryReplace bindingStore expectedOutputType = evalExprInner bindingStore tryReplace >> toFunctionalExpressionForTopLevel expectedOutputType