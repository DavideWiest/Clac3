module Clac3.P1.PatternReplacer

open Clac3.Expression
open Clac3.Type

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

type ValueReplacer = TAExpressionValue -> TAExpressionValue
type LiftedReplacer = TAExpression -> TAExpression

type TALift = TAExpressionValue -> TAExpression

type TALifter(
        atomLifter: Atom -> TAExpression,
        arrayLifter: TAExpression list -> TAExpression,
        nodeLifter: TAExpression list -> TAExpression
    ) = 
    
    member this.toTAExpr = function
        | TAAtom a -> atomLifter a
        | TAArray items -> arrayLifter items
        | TANode children -> nodeLifter children