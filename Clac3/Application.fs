module Clac3.Application

open Clac3.Domain

/// <summary>
/// Represents an application as a set of computation rules to a set of expressions.
/// The computation rules are combined into one object that is used to evaluate all the expressions.
/// Getting the args is separate from evaluting for performance measurement.
/// </summary>
[<AbstractClass>]
type Application<'a, 'b>(computationRules: 'a list, expressions: Expression list) =
    let computationRules = computationRules
    let expressions = expressions

    abstract member eval : args: 'b -> Expression list
    abstract member getEvalArgs : 'b

    member this.runProgram = this.getEvalArgs |> this.eval