open Clac3.Domain
open Clac3.DomainUtil
open Clac3.BuiltIn
open Clac3.MacroInterpreter.Application
open Clac3.Data
open Clac3.Interpreter2.MacroGrouper

let customRules: Macro list = [
    {
        pattern = pNC [vKw "factorial"; vInt 0]
        replacer = aInt 1
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
                ]
            ]
        )
    }
    {
        pattern = Value (PDefined (Value (PVariable (Value "x"))))
        replacer = aInt 5
    }
    {
        pattern = pNC [vKw "fibonacci"; vInt 0]
        replacer = aInt 0
    }
    {
        pattern = pNC [vKw "fibonacci"; vInt 1]
        replacer = aInt 1
    }
    {
        pattern = pNC [vKw "fibonacci"; pInt]
        replacer = Args.one (fun n ->
            Node [
                Node [
                    aKw "fibonacci";
                    Node [
                        n;
                        aKw "-";
                        aInt 1
                    ]
                ];
                aKw "+";
                Node [
                    aKw "fibonacci";
                    Node [
                        n;
                        aKw "-";
                        aInt 2
                    ]
                ]
            ]
        )
    }
    {
        pattern = pNC [vKw "fibonacciWithIf"; pInt]
        replacer = Args.one (fun n ->
            Node [
                aKw "if";
                Node [n; aKw "="; aInt 0]
                aKw "then";
                aInt 0;
                aKw "else";
                Node [
                    aKw "if";
                    Node [n; aKw "="; aInt 1]
                    aKw "then";
                    aInt 1;
                    aKw "else";
                    Node [
                        Node [
                            aKw "fibonacciWithIf";
                            Node [
                                n;
                                aKw "-";
                                aInt 1
                            ]
                        ];
                        aKw "+";
                        Node [
                            aKw "fibonacciWithIf";
                            Node [
                                n;
                                aKw "-";
                                aInt 2
                            ]
                        ]
                    ]
                ]
            ]
        )
    }
]

let app = ExtendedMacroApplication(customRules,
    [Node [aKw "fibonacci"; aInt 25]]
)

//let args = app.getEvalArgs
//let time = Performance.measureTime (fun () -> app.eval args)
//printfn "Results: \n%s" (time |> fst |> List.map ToString.expression |> String.concat "\n")
//printfn "Time: %ims" (time |> snd |> int)

groupByPattern coreRuleSet