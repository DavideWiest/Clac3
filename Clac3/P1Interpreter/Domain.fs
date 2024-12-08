module Clac3.P1Interpreter.Domain

open Clac3.Expression

// Patterns

type PatternUnion<'a> = 
    | Value of 'a
    | Any
    // | Collector

type AtomPattern = 
    | PBool of PatternUnion<bool>
    | PInteger of PatternUnion<int>
    | PFloat of PatternUnion<float>
    | PString of PatternUnion<string>
    | PVariable of PatternUnion<string>
    | PKeyword of PatternUnion<string>

type ExpressionPattern =
    | PAtom of PatternUnion<AtomPattern>
    | PList of PatternUnion<Pattern list>
    | PNode of PatternUnion<Pattern list>

and Pattern = PatternUnion<ExpressionPattern>

// TypeAnnotatedExpression

type Type =
    | TBool
    | TInteger
    | TFloat
    | TString
    | TVariable
    | TKeyword
    | TList of Type

type TypeAnnotatedExpression =
    | TAAtom of Atom
    // list is a recursive type, but won't evaluate to some other type 
    // the annotation is only for node, as its the only option that can evaluate to a different type
    | TAList of TypeAnnotatedExpression list
    | TANode of TypeAnnotatedExpression list * Type

// Rewrite rules

type Replacer = Expression list -> Expression

type RewriteRule = {
    pattern: Pattern
    replacer: Replacer
}