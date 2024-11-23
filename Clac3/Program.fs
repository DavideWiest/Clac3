open Clac3.Domain
open Clac3.DomainUtil
open Clac3.Interpreter
open Clac3.BuiltIn

let customRules: RewriteRule list = [
    {
        pattern = ValueNode ("factorial", 0)
        replacement = Args.zero 1
    };
    {
        pattern = GetNode ("factorial", TInteger)
        replacement = Args.one (fun n ->
            node (
                n, 
                "*",
                node (
                    "factorial",
                    node (n, "-", 1)
                )
            )
        )
    };
    {
        pattern = ValueNode (Keyword "fibonacci", Integer 0)
        replacement = Args.zero 0
    };
    {
        pattern = ValueNode (Keyword "fibonacci", Integer 1)
        replacement = Args.zero 1
    };
    {
        pattern = GetNode (Keyword "fibonacci", TInteger)
        replacement = Args.one (fun n ->
            node (
                node (
                    Keyword "fibonacci",
                    node (n, Keyword "-", Integer 1)
                ),
                Keyword "+",
                node (
                    Keyword "fibonacci",
                    node (n, Keyword "-", Integer 2)
                )
            )
        )
    }
]

let dummyProgram = {
    rewriteRules = List.append coreRuleSet customRules
    freeExpressions = [
        node (Keyword "factorial", Integer 3)
    ]
}

let result = evalProgram dummyProgram

printfn "\n---\nRESUlT:\n%A" result