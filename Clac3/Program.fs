open Clac3.Domain
open Clac3.DomainUtil
open Clac3.Interpreter
open Clac3.BuiltIn
open Clac3.Data

let customRules: RewriteRule list = [
    {
        pattern = pNC [vKw "factorial"; vInt 0]
        replacer = Args.zero (Integer 1)
    };
    {
        pattern = pNC [vKw "factorial"; pInt]
        replacer = Args.one (fun n ->
            Node [
                n;
                Keyword "*";
                Node [
                    Keyword "factorial";
                    Node [
                        n;
                        Keyword "-";
                        Integer 1
                    ]
                ]]
        )
    };
    {
        pattern = pNC [vKw "fibonacci"; vInt 0]
        replacer = Args.zero (Integer 0)
    };
    {
        pattern = pNC [vKw "fibonacci"; vInt 1]
        replacer = Args.zero (Integer 1)
    };
    {
        pattern = pNC [vKw "fibonacci"; pInt]
        replacer = Args.one (fun n ->
            Node [
                Node [
                    Keyword "fibonacci";
                    Node [
                        n;
                        Keyword "-";
                        Integer 1
                    ]
                ];
                Keyword "+";
                Node [
                    Keyword "fibonacci";
                    Node [
                        n;
                        Keyword "-";
                        Integer 2
                    ]
                ]
            ]
        )
    };
    {
        pattern = pNC [vKw "fibonacciWithIf"; pInt]
        replacer = Args.one (fun n ->
            Node [
                Keyword "if";
                Node [n; Keyword "="; Integer 0]
                Keyword "then";
                Integer 0;
                Keyword "else";
                Node [
                    Keyword "if";
                    Node [n; Keyword "="; Integer 1]
                    Keyword "then";
                    Integer 1;
                    Keyword "else";
                    Node [
                        Node [
                            Keyword "fibonacciWithIf";
                            Node [
                                n;
                                Keyword "-";
                                Integer 1
                            ]
                        ];
                        Keyword "+";
                        Node [
                            Keyword "fibonacciWithIf";
                            Node [
                                n;
                                Keyword "-";
                                Integer 2
                            ]
                        ]
                    ]
                ]
            ]
        )
    }
    {
        pattern = PLeaf (PVariableValue "x")
        replacer = Args.zero (Integer 5)
    }
]

let dummyProgram = {
    rewriteRules = List.append coreRuleSet customRules
    freeExpressions = [
        //Variable "x"
        //Node [Keyword "fibonacci"; Integer 25]
        Node [Keyword "fibonacciWithIf"; Integer 25]
    ]
}
let args = getEvalArgs dummyProgram
let time = Performance.measureTime (fun () -> evalProgram args)

printfn "Result: %A" time