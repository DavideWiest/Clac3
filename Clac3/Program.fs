open Clac3.P1.Expression
open Clac3.P1.Domain
open Clac3.P1.DomainUtil
open Clac3.P1.Application
open Clac3.P2.FExpression
open Clac3.P2.Application
open Clac3.P2.Function
open Clac3.P2.DomainUtil
open Clac3.Data

let customRules: RewriteRule list = [
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
]

let factorialFn = {
    ident = "fibonacci";
    lambda = Custom (
        [|"x"|],
        FBranch (
            FCall ("intEquals", [|FCall("x", [||]); FAtom(FInteger 0)|]),
            FAtom (FInteger 0),
            FBranch (
                FCall ("intEquals", [|FCall("x", [||]); FAtom(FInteger 1)|]),
                FAtom (FInteger 1),
                FCall ("addInts", [|
                    FCall ("fibonacci", [|
                        FCall ("subInts", [|FCall("x", [||]); FAtom(FInteger 1)|])
                    |])
                    FCall ("fibonacci", [|
                        FCall ("subInts", [|FCall("x", [||]); FAtom(FInteger 2)|])
                    |])
                |])
            )
        )
    )
}

let appRules = ExtendedRewriteRuleApplication(customRules,
    [Node [aKw "fibonacci"; aInt 25]]
)

let appFunc = ExtendedFunctionalApplication([|factorialFn|],
    [|FCall ("fibonacci", [|faInt 25|])|]
)

let tryFibRewriteRules () = 
    let args = appRules.getEvalArgs
    let result = Performance.measureTime (fun () -> appRules.eval args)
    printfn "Results: \n%s" (result |> fst |> Seq.map ToString.expression |> String.concat "\n")
    printfn "Time: %ims" (result |> snd |> int)

let tryFibFunctional () =
    let args = appFunc.getEvalArgs
    let result = Performance.measureTime (fun () -> appFunc.eval args)
    printfn "Results: \n%s" (result |> fst |> Seq.map P2ToString.atom |> String.concat "\n")
    printfn "Time: %ims" (result |> snd |> int)

tryFibFunctional ()