module Clac3.P1.Interpreter

open Clac3.Constants
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
    | [] -> TUnit
    | head::args ->
        match head with
        | { eType=TFunc _; expr=_ } -> failwith "todo"
        | { eType=TKeyword; expr=Atom (Keyword fnName) } ->
            match bindingStore.TryFind fnName with
            | None -> failwithf "Function %A not found" fnName
            | Some (binding) ->
                let argTypes = args |> List.map (fun a -> a.eType)

                List.zip binding.signature.args.[..argTypes.Length - 1] argTypes 
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
    | Array arrRaw -> 
        let arr = arrRaw |> Array.map (evalExprInner bindingStore tryReplace)

        let types = arr |> Array.map (fun a -> a.eType)
        let items = arr |> Array.map (fun a -> a.expr)

        if (types |> Array.distinct |> Array.length) > 1 then failwithf "Array elements must have the same type. Array: %A" arr

        { expr=Array items; eType=TArray arr[0].eType }
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
    | Atom a ->
        match a with
        | Bool b -> b |> FBool |> FAtom
        | Integer i -> i |> FInteger |> FAtom
        | Float f -> f |> FFloat |> FAtom
        | String s -> s |> FString |> FAtom
        | Variable v -> FRef { ident=v; args=[||] }
        | Keyword k -> failwith "Tried to convert a Expr to FExpr. Keywords should not be present within functional expressions."
    | Array arr -> FArray (arr |> Array.map toFunctionalExpression)
    | Node [] -> FUnit
    | Node (head::tail) ->
        match head with
        | Atom (Keyword fnName) -> 
            if fnName = branchIdent then
                match tail with
                | [cond; trueB; falseB] -> FBranch { cond=toFunctionalExpression cond; trueB=toFunctionalExpression trueB; falseB=toFunctionalExpression falseB }
                | _ -> failwithf "Expected 3 arguments for %s, got: %A" branchIdent tail
            else
                FRef { ident=fnName; args=tail |> List.map toFunctionalExpression |> Array.ofList }
        | _ -> failwithf "Expected keyword, got: %A. Full expression: %A" head (Node (head::tail))

let toFunctionalExpressionForTopLevel (expectedOutputType: Type option) (taExpr: TAExpression) = 
    if expectedOutputType.IsSome && taExpr.eType <> expectedOutputType.Value then failwithf "Expected type %A, got %A" expectedOutputType taExpr.eType
    toFunctionalExpression taExpr.expr, (expectedOutputType |> Option.defaultValue taExpr.eType)

let evalExpr bindingStore tryReplace expectedOutputType = evalExprInner bindingStore tryReplace >> toFunctionalExpressionForTopLevel expectedOutputType