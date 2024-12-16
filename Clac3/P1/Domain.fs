module Clac3.P1.Domain

open Clac3.Expression

type ProgramStatement = 
    | Definitiion of string * (string list) * Expression
    | Expression of Expression