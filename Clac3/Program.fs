open Clac3.Domain
open Clac3.DomainUtil
open Clac3.Interpreter
open Clac3.BuiltIn

let customRules: RewriteRule list = [
    {
        pattern = pNode ("factorial", 0)
        replacer = Args.zero 1
    };
    {
        pattern = pNode ("factorial", PInteger)
        replacer = Args.one (fun n ->
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
        pattern = pNode ("fibonacci", 0)
        replacer = Args.zero 0
    };
    {
        pattern = pNode ("fibonacci", 1)
        replacer = Args.zero 1
    };
    {
        pattern = pNode ("fibonacci", PInteger)
        replacer = Args.one (fun n ->
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
    {
        pattern = PVariableValue "x"
        replacer = Args.zero (Integer 5)
    }
]

let dummyProgram = {
    rewriteRules = customRules // List.append coreRuleSet customRules
    freeExpressions = [
        node("factorial", 0)
        // node (Keyword "fibonacci", Integer 12)
    ]
}

let result = evalProgram dummyProgram

printfn "\n---\nRESUlT:\n%A" result