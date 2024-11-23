module Clac3.Domain

type ExpressionType =
    | TBool
    | TInteger
    | TFloat
    | TString
    | TList
    | TVariable
    | TKeyword

    | TNode
    | TNodeContaining of Pattern list
    | TNodeStartingWith of Pattern list

    | TAny
    | TEvaluatedLeaf // any leaf that's not a variable

and Expression =
    | Bool of bool
    | Integer of int
    | Float of float
    | String of string
    | List of Expression list // this should probably be Expression instead
    | Variable of string
    | Keyword of string // only predefined symbols

    | Node of Expression list

and Pattern =
    | Get of ExpressionType
    | Value of Expression

type RewriteRule = {
    pattern: Pattern
    replacement: Expression list -> Expression
}

type Program = {
    rewriteRules: RewriteRule list;
    freeExpressions: Expression list;
}