module Clac3.Testing

open Clac3.Domain
open Clac3.DomainUtil
open Clac3.BuiltIn

let testRulesRaw = [
    {
        pattern = pNC [vKw "factorial"; vInt 0]
        replacer = Args.printAndPass >> Args.zero (aInt 1)
    }
    {
        pattern = pNC [vKw "factorial"; pInt]
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
        pattern = pNC [vKw "pow"; pInt; pInt]
        replacer = Args.two (fun a b -> pown (Args.getInt a) (Args.getInt b) |> aInt)
    }
    {
        pattern = pNC [vKw "replicateString"; pInt; pStr]
        replacer = Args.two(fun a b -> String.replicate (Args.getInt a) (Args.getString b) |> aStr)
    }
    {
        pattern = pNC [vKw "test"; pNC [vStr "Hello World!"]]
        replacer = Args.zero (aStr "Hello World! (base case)")
    }
    {
        pattern = pNC [vKw "test"; pNC [pStr]]
        replacer = Args.one (fun s -> aStr (Args.getString s + "(not base case)"))
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
]
