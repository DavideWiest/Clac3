module Clac3.Application

/// <summary>
/// Represents an application as a set of computation rules to a set of expressions.
/// The computation rules are combined into one object that is used to evaluate all the expressions.
/// Getting the args is separate from evaluting for performance measurement.
/// </summary>
[<AbstractClass>]
type Application<'a, 'b, 'c>() =
    abstract member eval : args: 'a -> 'c seq
    abstract member getEvalArgs : 'a

    member this.runProgram () = this.getEvalArgs |> this.eval