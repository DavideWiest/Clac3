module rec Clac3.P1.DecisionTree.Walker

open Clac3.Util
open Clac3.Expression
open Clac3.TypeAnnotatedExpression
open Clac3.P1.DecisionTree.Domain

// Atoms
let private tryFindInPatternMap wrapperFn (key: 'a) (patternMap: MaybePatternLeaf<'a>) =
    let withoutKey replacer = replacer, []
    let withKey replacer = replacer, [key |> wrapperFn |> Atom]

    patternMap 
    |> Option.bind (fun pdt -> 
        pdt.value
        |> List.tryFind (fun (k, _) -> k = key)
        |> Option.map snd
        |> Option.map withoutKey
        |> Option.orElse (pdt.any |> Option.map withKey)
    )
    
let private tryFindAtomReplacer tree = function
    | Bool b -> tree.bool |> tryFindInPatternMap Bool b
    | Integer i -> tree.integer |> tryFindInPatternMap Integer i
    | Float f -> tree.float |> tryFindInPatternMap Float f
    | String s -> tree.string |> tryFindInPatternMap String s
    | Variable v -> tree.variable |> tryFindInPatternMap Variable v
    | Keyword k -> tree.keyword |> tryFindInPatternMap Keyword k

let private walkAtom atom (pattern: PatternWrapper<AtomDecisionTree>) = 
    atom 
    |> tryFindAtomReplacer pattern.value 
    |> Option.orElse (Option.tupleWithRev pattern.any [Atom atom])

// Lists and Nodes
let private tryGetNodeTreeResultInner wrapperType (children: TypeAnnotatedExpression list) (flp: FirstLevelPattern, next: NodeDecisionTree) =
    walk flp children.Head
    |> Option.bind (fun (_, args) -> 
        children.Tail 
        |> walkNodeInner wrapperType next 
        |> Option.map (fun (replacer, argsTail) -> replacer, (args @ argsTail))
    )

let private walkNodeInner wrapperType pattern (children: TypeAnnotatedExpression list) = 
    // this order is crucial
    // anything that could cut of another pattern should be check later than that one
    // continuing rules first -- then ending rules -- then rest/collector rules
    if  children.Length > 0 then List.tryPick (tryGetNodeTreeResultInner wrapperType children) pattern.value else None
    |> Option.orElse (if children.Length = 1 then pattern.ending |> Option.bind (fun pattern -> walk pattern children.Head) else None)
    |> Option.orElse (if children.Length > 0 then pattern.rest |> Option.map (fun replacer -> replacer, [Node children]) else None)

let private walkNode wrapperType (children: TypeAnnotatedExpression list) (pattern: CollectablePatternWrapper<NodeDecisionTree>) = 
    children |> walkNodeInner wrapperType pattern.value |> Option.orElse (Option.tupleWithRev pattern.any [wrapperType children])
        
// Expressions
let private walkExpression tree = function
    | TAAtom (t,a) -> tree.atom |> Option.bind (walkAtom a)
    | TANode (t,children) -> tree.node |> Option.bind (walkNode TANode children)
    | TAList (t,children)-> tree.list |> Option.bind (walkNode TAList children)

// First level
let walk (tree: FirstLevelPattern) (expr: TypeAnnotatedExpression) = 
    tree.value |> Option.bind (fun v -> walkExpression v expr) |> Option.orElse (Option.tupleWithRev tree.any [expr])

type Walker(tree: FirstLevelPattern) = 
    member this.tryReplace expr = 
        Walker.walk tree expr |> Option.map (fun (replacer, args) -> replacer args)
