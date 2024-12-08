module Clac3.P1Interpreter.Application

open Clac3.Domain
open Clac3.Application
open Clac3.Interpreter
open Clac3.BuiltIn
open Clac3.P1Interpreter.DecisionTree

type MacroApplication(macros: RewriteRule list, expressions: Expression list) =
    inherit Application<RewriteRule, Walker>(macros, expressions)

    override this.eval tree = 
        let tryReplace (tree: Walker) expr = tree.tryReplace expr
        expressions |> List.map (evalExpr tree tryReplace 0)

    override this.getEvalArgs = Walker ((Builder macros).constructTree)

type ExtendedMacroApplication(extraMacros: RewriteRule list, exprs: Expression list) =
    inherit MacroApplication(List.append coreRuleSet extraMacros, exprs)
