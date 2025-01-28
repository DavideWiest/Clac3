module Clac3.P2.DomainUtil

open Clac3.Util
open Clac3.Constants
open Clac3.Expression
open Clac3.Type
open Clac3.FunctionalExpression
open Clac3.Binding
open Clac3.P1.Domain

module rec P2ToString =
    let atom = function
        | FInteger i -> string i
        | FFloat f -> string f
        | FBool b -> string b
        | FString s -> "\"" + s + "\""

    let private expressionInner (bindingRelation: Map<int, string>) = function
        | S2.FExpression.FUnit -> "()"
        | S2.FExpression.FAtom a -> atom a
        | S2.FExpression.FArray a -> a |> node bindingRelation |> ToString.inBrackets
        | S2.FExpression.FRef { ident=ident; args=args } -> 
            args |> node bindingRelation |> prepend bindingRelation[ident] |> ToString.inParans
        | S2.FExpression.FBranch { cond = cond; trueB = trueB; falseB = falseB } -> 
            { ident=(-1); args=[|cond;trueB;falseB|] } |> S2.FExpression.FRef |> expressionInner bindingRelation

    let expression (bindingRelationOrig: Map<int, string>) expr =
        let bindingRelation = Map.add -1 branchIdent bindingRelationOrig
        match expr with
        | S2.FExpression.FRef { ident=ident; args=args } -> node bindingRelation args |> prepend bindingRelation[ident]
        | expr -> expressionInner bindingRelation expr

    let node bindingRelation = Array.map (expressionInner bindingRelation) >> String.concat " "
    let prepend a s = if s = "" then a else sprintf "%s %s" a s


module Interpreter = 
    let getReferenceStore (baseBindings: S1.Binding array) (definitions: RawBinding array) : ReferenceStore = 
        let baseSignatureMap = 
            baseBindings 
            |> Array.map (fun b -> b.ident, b.signature) 

        let customSignatureMap =
            definitions 
            |> Array.map (fun d -> d.ident, d.signature)

        // suffices for flat bindings, but not nested ones
        let referenceStore = 
            definitions
            |> Array.choose (fun d -> 
                match d.body with
                | RCustom { argIdents = argIdents; body = _ } -> 
                    let oneIdentMap = 
                        argIdents 
                        |> Array.mapi (fun i ident -> ident, d.signature.args[i]) 
                        |> Map.ofArray
                    
                    Some ([d.ident], oneIdentMap)
                | RValue _ -> None // a value doesn't have any arguments
            )
        
        let referenceStoreLowestLevel = 
            baseSignatureMap
            |> Array.append customSignatureMap
            |> Array.map (fun (k, signature) -> k, TLambda signature)
            |> Map.ofArray

        referenceStore
        |> Array.append [| ([], referenceStoreLowestLevel) |]
        |> Map.ofArray

    let getBindingMap (bindings: S1.Binding array) =    
        bindings |> Array.map (fun b -> b.ident, b) |> Map.ofArray


let toBinding (ident, signatureList: Type list, binding) : S1.Binding = 
    if signatureList.Length = 0 then failwithf "Signature requires at least 1 type. Got: %A" signatureList
    let argTypes, outputType = signatureList[..signatureList.Length-2], signatureList[signatureList.Length-1]

    {
        ident = ident
        signature = { args = argTypes; returnType = outputType }
        binding = binding
    }

let toCustomFn (argIdents, body) = Custom { argIdents = Array.ofList argIdents; body = body }

let toValueReference ident = S1.FExpression.FRef { ident = ident; args = [||] }
let toReference (ident, args) = S1.FExpression.FRef { ident = ident; args = args }
let toBranch (cond, trueB, falseB) = S1.FExpression.FBranch { cond = cond; trueB = trueB; falseB = falseB }

let faBo = FBool >> S1.FExpression.FAtom
let faInt = FInteger >> S1.FExpression.FAtom
let faFl = FFloat >> S1.FExpression.FAtom
let faStr = FString >> S1.FExpression.FAtom

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
        | FAtom (FBool b) -> b
        | item -> Error.typeError "bool" item

    let getInt = function
        | FAtom (FInteger i) -> i
        | item -> Error.typeError "int" item

    let getFloat = function
        | FAtom (FFloat f) -> f
        | item -> Error.typeError "float" item

    let getString = function
        | FAtom (FString s) -> s
        | item -> Error.typeError "string" item

    let getArray = function
        | Array l -> l
        | item -> Error.typeError "list" item