module Clac3.P2.Normalizer

open Clac3.FExpression
open Clac3.Function

module Expression = 
    let rec switchFunctionCall (bindingIdentMap: Map<string, int>) (fc: S1.FunctionCall) = 
        fc |> fun (ident, args) -> bindingIdentMap[ident], args |> Array.map (switchBindingIdentOne bindingIdentMap)

    and switchBindingIdentOne (bindingIdentMap: Map<string, int>) (expr: S1.FExpression) : S2.FExpression = 
        match expr with
        | FAtom a -> S2.FExpression.FAtom a
        | FCall (ident, args) -> S2.FExpression.FCall(bindingIdentMap[ident], args |> Array.map (switchBindingIdentOne bindingIdentMap))
        | FBranch (cond, thenExpr, elseExpr) -> 
            S2.FExpression.FBranch(
                switchBindingIdentOne bindingIdentMap cond, 
                switchBindingIdentOne bindingIdentMap thenExpr,
                switchBindingIdentOne bindingIdentMap elseExpr
            )

    let switchBindingIdentAll bindingIdentMap (exprs: S1.FExpression array) = exprs |> Array.map (switchBindingIdentOne bindingIdentMap)

module Bindings = 
    let switchBindingIdent (bindingIdentMap: Map<string, int>) (bindingStore: S1.BindingStore) : S2.BindingStore = 
        let newBindingStore: S2.BindingStore = Array.create (bindingIdentMap |> Map.keys |> Seq.length) None
        
        bindingStore |> Map.iter (fun ident expr -> 
            match expr with
            | BValue v -> 
                newBindingStore.[bindingIdentMap[ident]] <- Some (S2.Binding.BValue v)
            | BFuncDef fn -> 
                let newFn = 
                    match fn.lambda with
                    | BuiltIn fn -> S2.FunctionBody.BuiltIn fn
                    | Custom (argIdents, body) -> S2.FunctionBody.Custom(argIdents |> Array.map (fun ident -> bindingIdentMap[ident]), body |> Expression.switchBindingIdentOne bindingIdentMap)
                
                newBindingStore.[bindingIdentMap[ident]] <- Some (S2.Binding.BFuncDef { ident = bindingIdentMap[ident]; lambda = newFn })

        )

        newBindingStore

let normalize (exprs: S1.FExpression array) (bindingStore: S1.BindingStore) =
    let nameBindingIdents = Map.keys bindingStore |> Array.ofSeq
    let argBindingIdents = Map.values bindingStore |> Array.ofSeq |> Array.collect (fun b -> 
        match b with
        | BFuncDef fn -> 
            match fn.lambda with
            | BuiltIn _ -> [||]
            | Custom (argIdents, _) -> argIdents
        | _ -> [||]
    )

    let bindingIdentMapitems = 
        Array.append nameBindingIdents argBindingIdents 
        |> Array.distinct 
        |> Array.mapi (fun i ident -> ident, i)

    let bindingIdentMap = bindingIdentMapitems |> Map.ofArray
    let bindingIdentMapRev =
        bindingIdentMapitems
        |> Array.map (fun (id, i) -> i, id)
        |> Map.ofArray

    let newExprs = Expression.switchBindingIdentAll bindingIdentMap exprs
    let newBindings = Bindings.switchBindingIdent bindingIdentMap bindingStore

    newExprs, newBindings, bindingIdentMapRev