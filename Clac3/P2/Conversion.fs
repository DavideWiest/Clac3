module Clac3.P2.Conversion

open Clac3.Constants
open Clac3.FunctionalExpression
open Clac3.Expression
open Clac3.Type

let private getExprValues = List.map (fun taExpr -> taExpr.expr)

let rec toFunctionalExpression = function
    | TAAtom a ->
        match a with
        | Bool b -> b |> FBool |> FAtom
        | Integer i -> i |> FInteger |> FAtom
        | Float f -> f |> FFloat |> FAtom
        | String s -> s |> FString |> FAtom
        | Variable v -> FRef { ident=v; args=[||] }
        | Keyword k -> failwith "Tried to convert a Expr to FExpr. Keywords should not be present within functional expressions."
    | TAArray arr -> arr |> getExprValues |> List.map toFunctionalExpression |> Array.ofList |> FArray
    | TANode [] -> FUnit
    | TANode (head::tail) ->
        match head with
        | { expr = TAAtom (Keyword fnName); eType = TKeyword } -> 
            if fnName = branchIdent then
                match tail with
                | [cond; trueB; falseB] -> FBranch { cond=toFunctionalExpression cond.expr; trueB=toFunctionalExpression trueB.expr; falseB=toFunctionalExpression falseB.expr }
                | _ -> failwithf "Expected 3 arguments for %s, got: %A" branchIdent tail
            else
                FRef { ident=fnName; args=tail |> getExprValues |> List.map toFunctionalExpression |> Array.ofList }
        | _ -> failwithf "Expected keyword, got: %A. Full expression: %A" head (TANode (head::tail))

let toFunctionalExpressionForTopLevel (expectedOutputType: Type option) (taExpr: TAExpression) = 
    if expectedOutputType <> None && expectedOutputType <> Some taExpr.eType then failwithf "Expected type %A, got %A" expectedOutputType taExpr.eType
    toFunctionalExpression taExpr.expr, (expectedOutputType |> Option.defaultValue taExpr.eType)