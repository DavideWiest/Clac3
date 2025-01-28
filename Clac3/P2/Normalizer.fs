module Clac3.P2.Normalizer

open Clac3.FunctionalExpression
open Clac3.Binding

module Expression = 
    let rec switchFunctionCall (bindingIdentMap: Map<string, int>) (fc: S1.Reference) = 
        bindingIdentMap[fc.ident], fc.args |> Array.map (switchBindingIdentOne bindingIdentMap)

    and switchBindingIdentOne (bindingIdentMap: Map<string, int>) (expr: S1.FExpression) : S2.FExpression = 
        match expr with
        | S1.FExpression.FUnit -> S2.FExpression.FUnit
        | FAtom a -> S2.FExpression.FAtom a
        | FArray a -> a |> Array.map (switchBindingIdentOne bindingIdentMap) |> S2.FExpression.FArray
        | FRef fc -> 
            S2.FExpression.FRef { 
                ident = bindingIdentMap[fc.ident]; 
                args = fc.args |> Array.map (switchBindingIdentOne bindingIdentMap) 
            }
        | FBranch branch -> 
            S2.FExpression.FBranch {
                cond = switchBindingIdentOne bindingIdentMap branch.cond;
                trueB = switchBindingIdentOne bindingIdentMap branch.trueB;
                falseB = switchBindingIdentOne bindingIdentMap branch.falseB
            }

    let switchBindingIdentAll bindingIdentMap (exprs: S1.FExpression array) = exprs |> Array.map (switchBindingIdentOne bindingIdentMap)

module Bindings = 
    let applyToInnerExprs (bindingIdentMap: Map<string, int>) = function
        | S1.BindingValue.BValue v -> S2.BindingValue.BValue (Expression.switchBindingIdentOne bindingIdentMap v)
        | BuiltIn fn -> S2.BindingValue.BuiltIn fn
        | Custom customFn ->
            let newArgIdents = customFn.argIdents |> Array.map (fun ident -> bindingIdentMap[ident])
            let newBody = customFn.body |> Expression.switchBindingIdentOne bindingIdentMap
                
            S2.BindingValue.Custom { argIdents = newArgIdents; body = newBody }
    
    let switchBindingIdent (bindingIdentMap: Map<string, int>) (bindingStore: S1.BindingStore) : S2.BindingStore = 
        let newBindingStore: S2.BindingStore = Array.create (bindingIdentMap |> Map.keys |> Seq.length) None
        
        bindingStore |> Map.iter (fun ident binding -> 
            newBindingStore.[bindingIdentMap[ident]] <- Some (applyToInnerExprs bindingIdentMap binding.binding)
        )

        newBindingStore

let normalize (exprs: S1.FExpression array) (bindingStore: S1.BindingStore) =
    let nameBindingIdents = Map.keys bindingStore |> Array.ofSeq
    let argBindingIdents = Map.values bindingStore |> Array.ofSeq |> Array.collect (fun b -> 
        match b.binding with
        | Custom customFn -> customFn.argIdents
        | _ -> [||]
    )

    let bindingIdentMapItems = 
        Array.append nameBindingIdents argBindingIdents 
        |> Array.distinct 
        |> Array.mapi (fun i ident -> ident, i)

    let bindingIdentMap = bindingIdentMapItems |> Map.ofArray

    let bindingIdentMapRev =
        bindingIdentMapItems
        |> Array.map (fun (id, i) -> i, id)
        |> Map.ofArray

    let newExprs = Expression.switchBindingIdentAll bindingIdentMap exprs
    let newBindings = Bindings.switchBindingIdent bindingIdentMap bindingStore

    newExprs, newBindings, bindingIdentMapRev