module Clac3.Domain

// Patterns
type PatternUnion<'a> = 
    | Value of 'a
    | Any of string // the variable name that the value is bound to

type AtomPattern = 
    | PBool of PatternUnion<bool>
    | PInteger of PatternUnion<int>
    | PFloat of PatternUnion<float>
    | PString of PatternUnion<string>
    | PVariable of PatternUnion<string>
    | PKeyword of PatternUnion<string>

type ExpressionPattern =
    | PDefined of PatternUnion<AtomPattern>
    | PList of PatternUnion<Pattern list>
    | PNode of PatternUnion<Pattern list>

and Pattern = PatternUnion<ExpressionPattern>

// Signature

// no keywords: they are combined and put to the front
// no variables: they are supposed to be replaced by the value they represent before applying a rule

type AtomSignature = 
    | SBool
    | SInteger
    | SFloat
    | SString

type ExpressionSignature =
    | SDefined of AtomSignature
    | SList of ExpressionSignature list
    | SNode of ExpressionSignature list

// Expressions
type DefinedAtom = 
    | Bool of bool
    | Integer of int
    | Float of float
    | String of string

type SymbolAtom = 
    | Variable of string
    | Keyword of string

type Expression =
    | Defined of DefinedAtom
    | Symbol of SymbolAtom
    | List of Expression list
    | Node of Expression list

// Computation rules
type Macro = {
    pattern: Pattern
    replacer: Expression
}