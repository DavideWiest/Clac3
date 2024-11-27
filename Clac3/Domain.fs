module Clac3.Domain

type PatternValue<'a> = 
    | Value of 'a
    | Any

type LeafPattern = 
    | PBool of PatternValue<bool>
    | PInteger of PatternValue<int>
    | PFloat of PatternValue<float>
    | PString of PatternValue<string>
    | PVariable of PatternValue<string>
    | PKeyword of PatternValue<string>

type Pattern =
    | PLeaf of PatternValue<LeafPattern>
    | PList of PatternValue<Pattern list>
    | PNode of PatternValue<Pattern list>

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

type Replacer = Expression list -> Expression

type RewriteRule = {
    pattern: Pattern
    replacer: Replacer
}

type Program = {
    rewriteRules: RewriteRule list;
    freeExpressions: Expression list;
}