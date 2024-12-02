module Clac3.Domain

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

// Expressions
type Atom = 
    | Bool of bool
    | Integer of int
    | Float of float
    | String of string
    | Variable of string
    | Keyword of string

and Expression =
    | Atom of Atom
    | List of Expression list
    | Node of Expression list

// Computation rules
type Replacer = Expression list -> Expression

type RewriteRule = {
    pattern: Pattern
    replacer: Replacer
}