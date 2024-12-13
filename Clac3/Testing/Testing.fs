module Clac3.Testing.Testing

open Clac3.P1.Expression
open Clac3.P1.RewriteRule
open Clac3.P1.DomainUtil
open Clac3.P1.BuiltIn
open Clac3.P1.Application

let testRulesRaw = [
    {
        pattern = NC [vKw "factorial"; vInt 0]
        replacer = Args.printAndPass >> Args.zero (aInt 1)
    }
    {
        pattern = NC [vKw "factorial"; pInt]
        replacer = Args.one (fun n ->
            Node [
                n;
                aKw "*";
                Node [
                    aKw "factorial";
                    Node [
                        n;
                        aKw "-";
                        aInt 1
                    ]
                ]]
        )
    }
    {
        pattern = Value (PAtom (Value (PVariable (Value "x"))))
        replacer = Args.zero (aInt 5)
    }
    {
        pattern = NC [vKw "pow"; pInt; pInt]
        replacer = Args.two (fun a b -> pown (Args.getInt a) (Args.getInt b) |> aInt)
    }
    {
        pattern = NC [vKw "replicateString"; pInt; pStr]
        replacer = Args.two(fun a b -> String.replicate (Args.getInt a) (Args.getString b) |> aStr)
    }
    {
        pattern = NC [vKw "test"; NC [vStr "Hello World!"]]
        replacer = Args.zero (aStr "Hello World! (base case)")
    }
    {
        pattern = NC [vKw "test"; NC [pStr]]
        replacer = Args.one (fun s -> aStr (Args.getString s + "(not base case)"))
    }
    {
        pattern = NCC [pInt; vKw "plus"]
        replacer = Args.two (fun a rest -> Node [aKw "plus"; aInt (Args.getInt a); Node (Args.getNode rest)])
    }
]

let testRules = testRulesRaw |> List.map (fun r -> { r with replacer = Args.printAndPass >> r.replacer }) |> List.append arithmeticRules
let  testExprs = [
    //Atom (Variable "x") // search that resolves fast and doesn't extract values
    Node [aKw "factorial"; aInt 0] // base case
    Node [aKw "factorial"; aInt 5] // extraction of 1 value
    Node [aKw "pow"; aInt 2; aInt 5] // extraction of two values
    Node [aKw "replicateString"; aInt 3; aStr "Hello World (replicated)"] // extraction of two values by type
    Node [aKw "test"; Node [aStr "Hello World!"]] // nested search
    Node [aKw "test"; Node [aStr "other string"]] // nested search and extracting values
    Node [aInt 1; aKw "plus"; aInt 2; aKw "plus"; aInt 3]
]

let runTestsPhase1 () =
    let ruleTestApp = RewriteRuleApplication(testRules, testExprs)
    ruleTestApp.runProgram |> Seq.iter (fun (result) -> printfn "%s" (ToString.expression result))