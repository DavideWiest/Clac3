module Clac3.MacroInterpreter.Application

open Clac3.Domain
open Clac3.Application
open Clac3.DecisionTree
open Clac3.Interpreter
open Clac3.BuiltIn

type MacroApplication(macros: Macro list, expressions: Expression list) =
    inherit Application<Macro, Walker>(macros, expressions)

    override this.eval tree = 
        let tryReplace (tree: Walker) expr = tree.tryReplace expr
        expressions |> List.map (evalExpr tree tryReplace 0)

    override this.getEvalArgs = Walker ((Builder macros).constructTree)

type ExtendedMacroApplication(extraMacros: Macro list, exprs: Expression list) =
    inherit MacroApplication(List.append coreRuleSet extraMacros, exprs)
