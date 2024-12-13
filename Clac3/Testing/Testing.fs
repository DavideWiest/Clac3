module Clac3.Testing.Testing

open Clac3.Expression
open Clac3.P1.RewriteRule
open Clac3.P1.DomainUtil
open Clac3.P1.BuiltIn
open Clac3.P1.Application

let testRulesRaw = [
    {
        pattern = NC [vKw "factorial"; vInt 0]
        replacer = Args.zero (aInt 1)
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
                ]
            ]
        )
    }
    {
        pattern = Value (PAtom (Value (PVariable (Value "x"))))
        replacer = Args.zero (aInt 5)
    }
    {
        pattern = NC [vKw "test"; NC [vStr "Hello World!"]]
        replacer = Args.zero (aStr "Hello World! (base case)")
    }
    {
        pattern = NCC [pInt; vKw "plus"; pInt]
        replacer = Args.three (fun a b rest -> Node ([Node [aKw "plus"; a; b]]@(Args.getChildren rest)))
    }
]

let testRules = testRulesRaw |> List.map (fun r -> { r with replacer = Args.printAndPass >> r.replacer }) |> List.append arithmeticRules
let  testExprs = [
    Node [aKw "factorial"; aInt 0] // base case
    Node [aKw "factorial"; aInt 5] // extraction of 1 value
    Node [aKw "replicateString"; aInt 3; aStr "Hello World (replicated)"] // won't match
    Node [aKw "test"; Node [aStr "Hello World!"]] // nested search
    Node [aKw "test"; Node [aStr "other string"]] // won't match
    Node [aInt 1; aKw "plus"; aInt 2; aKw "plus"; aInt 3] // nesting a node
]

let runTestsPhase1 () =
    let ruleTestApp = RewriteRuleApplication(testRules, testExprs)
    ruleTestApp.runProgram |> Seq.iter (fun (result) -> printfn "%s" (ToString.expression result))