module Clac3.P1.Domain

open Clac3.Expression
open Clac3.Type
open Clac3.FunctionalExpression

type ProgramStatement = 
    | Definitiion of string * (string list) * Expression
    | Expression of Expression

type FunctionalProgramStatement = 
    | FDefinitiion of string * (string list) * S1.FExpression
    | FExpression of Type * S1.FExpression