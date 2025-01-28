module Clac3.P1.PatternReplacer

open Clac3.Expression
open Clac3.Type
open Clac3.Binding

// Patterns

type SimplePatternUnion<'a> = 
    | Value of 'a
    | Any

type PatternUnion<'a> = 
    | ConstantValue of 'a
    | Value of 'a // also includes nodes that result in such a value
    | Any

type AtomPattern = 
    | PBool of PatternUnion<bool>
    | PInteger of PatternUnion<int>
    | PFloat of PatternUnion<float>
    | PString of PatternUnion<string>

    | PVariable of PatternUnion<string>
    | PKeyword of PatternUnion<string>

type ExpressionPattern =
    | PAtom of PatternUnion<AtomPattern>
    | PNode of SimplePatternUnion<CollectablePattern list>
    | PArray of SimplePatternUnion<CollectablePattern list>
    | PLambda of SimplePatternUnion<FnSignature>

and Pattern = PatternUnion<ExpressionPattern>
and CollectablePattern =
    | CValue of Pattern
    | CRest

// Rewrite rules

type ProtoReplacer = TAExpression list -> TAExpressionValue

type RewriteRule = {
    pattern: Pattern
    replacer: ProtoReplacer
}

type ValueReplacer = CtxPath -> TAExpressionValue -> TAExpressionValue
type LiftedReplacer = CtxPath -> TAExpression -> TAExpression

type TALift = CtxPath -> TAExpressionValue -> TAExpression

type TALifter(
        atomLifter: CtxPath -> Atom -> TAExpression,
        arrayLifter: CtxPath -> TAExpression list -> TAExpression,
        nodeLifter: CtxPath -> TAExpression list -> TAExpression
    ) = 
    
    member this.toTAExpr ctxPath = function
        | TAAtom a -> atomLifter ctxPath a
        | TAArray items -> arrayLifter ctxPath items
        | TANode children -> nodeLifter ctxPath children