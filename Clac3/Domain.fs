module Clac3.Domain

type Pattern =
    | PAny
    | PAnyEvaluatdLeaf // any leaf that's not a variable
    | PBool
    | PInteger
    | PFloat
    | PString
    | PList
    | PVariable
    | PKeyword
    | PNode

    | PBoolValue of bool
    | PIntegerValue of int
    | PFloatValue of float
    | PStringValue of string
    | PListValue of Pattern list
    | PVariableValue of string // PVariable has to exist, because variables exist
    | PKeywordValue of string
    | PNodeContaining of Pattern list
    // not yet implemented in the interpreter
    //| PNodeStartingWith of Pattern list
    
type Expression =
    | Bool of bool
    | Integer of int
    | Float of float
    | String of string
    | List of Expression list
    | Variable of string
    | Keyword of string

    | Node of Expression list

type RewriteRule = {
    pattern: Pattern
    replacer: Expression list -> Expression
}

type Program = {
    rewriteRules: RewriteRule list;
    freeExpressions: Expression list;
}