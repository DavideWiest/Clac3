open Clac3.Domain
open Clac3.BuiltIn
open Clac3.Interpreter

let customRules = [
    {
        pattern = function 
            | Node [Leaf(Keyword "factorial"); Leaf(Integer 0)] -> IntLeaf 1
            | Node [Leaf(Keyword "factorial"); Leaf(x)] -> 
                [
                    Leaf x;
                    Leaf(Keyword "*"); 
                    Node([
                        Leaf(Keyword "factorial"); 
                        Node([Leaf x; Leaf(Keyword "-"); Leaf(Integer 1)])
                    ])
                ]
                |> Node
                |> Some
            | _ -> None
    }
    {
        pattern = function 
            | Node [Leaf(Keyword "fibonacci"); Leaf(Integer 0)] -> IntLeaf 0
            | Node [Leaf(Keyword "fibonacci"); Leaf(Integer 1)] -> IntLeaf 1
            | Node [Leaf(Keyword "fibonacci"); n] -> 
                [
                    Node([Leaf(Keyword "fibonacci"); Node([n; Leaf(Keyword "-"); Leaf(Integer 1)])]);
                    Leaf(Keyword "+");
                    Node([Leaf(Keyword "fibonacci"); Node([n; Leaf(Keyword "-"); Leaf(Integer 2)])])
                ]
                |> Node
                |> Some
            | _ -> None
    
    }
]

let dummyProgram = {
    rewriteRules = coreRuleSet::customRules
    freeExpressions = [
        // Node([Leaf(Keyword "fibonacci"); Leaf(Integer 30)])
        Node([Leaf(Keyword "factorial"); Leaf(Integer 3)])
    ]
}

let result = evalProgram dummyProgram

printfn "\n---\nRESUlT:\n%A" result