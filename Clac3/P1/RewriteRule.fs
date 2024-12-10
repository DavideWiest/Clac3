module Clac3.P1.Domain

open Clac3.P1.Expression

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

// Rewrite rules

type Replacer = Expression list -> Expression

type RewriteRule = {
    pattern: Pattern
    replacer: Replacer
}