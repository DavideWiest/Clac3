﻿module Clac3.P1.RewriteRule

open Clac3.Expression

// Patterns

type PatternUnion<'a> = 
    | Value of 'a
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
    | PNode of PatternUnion<CollectablePattern list>
    | PArray of PatternUnion<CollectablePattern list>

and Pattern = PatternUnion<ExpressionPattern>
and CollectablePattern =
    | CValue of Pattern
    | CRest

// Rewrite rules

type Replacer = Expression list -> Expression

type RewriteRule = {
    pattern: Pattern
    replacer: Replacer
}