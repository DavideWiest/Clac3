module Clac3.P2.Lift

open Clac3.Constants
open Clac3.Expression
open Clac3.Type
open Clac3.Binding
open Clac3.P1.PatternReplacer
open Clac3.P1.DomainUtil

let private inferAndValidateTypeForArray = function
    | [] -> TArray None
    | head::args -> 
        let headType = head.eType
        args |> List.iter (fun a -> if not (typesMatch a.eType headType) then failwithf "Array elements must have the same type. Types: %A" (args |> List.map (fun a -> a.eType)))
        headType |> Some |> TArray

let rec private inferAndValidateTypeForNodes (referenceStore: ReferenceStore) ctxPath = function
    | [] -> TUnit
    | head::args ->
        match head with
        | { eType=TKeyword; expr=TAAtom (Keyword fnName) } ->
            if fnName = branchIdent then
                match args with
                | [cond; trueB; falseB] -> 
                    let condType = cond.eType
                    if not (typesMatch condType TBool) then failwithf "Expected type %A for condition, got %A" TBool condType
                    let trueType = trueB.eType
                    let falseType = falseB.eType
                    if not (typesMatch trueType falseType) then failwithf "Expected equal types for true and false branches, got %A and %A" trueType falseType
                    trueType
                | _ -> failwithf "Expected 3 arguments for Branching-function %s, got %i: %A" branchIdent (args.Length) args
            else

            match accessReferenceStore referenceStore ctxPath fnName with
            | None -> 
                failwithf "Function %A not found. Trying to lift following expression-value to TAExpression: %A" fnName (TANode (head::args))
            | Some (TLambda signature) ->
                let argTypes = args |> List.map (fun a -> a.eType)

                List.zip signature.args.[..argTypes.Length - 1] argTypes 
                |> List.iter (fun (tExpected, tActual) -> 
                    if not (typesMatch tExpected tActual) then failwithf "Expected type %A, got %A when calling %A" tExpected tActual fnName
                )

                if signature.args.Length > argTypes.Length 
                then failwithf "Got too many args for %A: Expected %A, got %A" fnName signature.args.Length argTypes.Length

                let argTypesRemaining = signature.args[args.Length..]

                if argTypesRemaining.IsEmpty
                then signature.returnType 
                else TLambda { args=argTypesRemaining; returnType=signature.returnType }
            | Some _ -> failwithf "Expected a function for %A, got: %A" fnName head
        // with the denesting rules, node with 1-items will be denested and need not be handled separately
        // also, nodes like ((...) ...) will also be denested (to (... ...) )
        | _ -> failwithf "Evaluated node in phase 1 must be convertible to functional form. Expected function or keyword, got: %A" (TANode (head::args))

let private liftArray _ items = { expr=TAArray items; eType=inferAndValidateTypeForArray items }
let private liftNode referenceStore ctxPath children = { expr=TANode children; eType=inferAndValidateTypeForNodes referenceStore ctxPath children }

let functionalLift (referenceStore: ReferenceStore) = TALifter(Lift.liftAtom referenceStore, liftArray, liftNode referenceStore)