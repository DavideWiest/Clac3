open Clac3.Expression
open Clac3.Representation
open Clac3.P1Interpreter.Domain
open Clac3.P1Interpreter.DomainUtil
open Clac3.P1Interpreter.Application
open Clac3.Data

let customRules: RewriteRule list = [
    {
        pattern = pNC [vKw "factorial"; vInt 0]
        replacer = Args.zero (aInt 1)
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
        pattern = pNC [vKw "fibonacci"; vInt 0]
        replacer = Args.zero (aInt 0)
    }
    {
        pattern = pNC [vKw "fibonacci"; vInt 1]
        replacer = Args.zero (aInt 1)
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

let app = ExtendedRewriteRuleApplication(customRules,
    [Node [aKw "fibonacci"; aInt 25]]
)

let args = app.getEvalArgs
let time = Performance.measureTime (fun () -> app.eval args)
printfn "Results: \n%s" (time |> fst |> List.map ToString.expression |> String.concat "\n")
printfn "Time: %ims" (time |> snd |> int)
