
open Clac3.Application
open Clac3.P1.Expression
open Clac3.P1.RewriteRule
open Clac3.P1.DomainUtil
open Clac3.P1.Application
open Clac3.P2.FExpression
open Clac3.P2.Application
open Clac3.P2.Function
open Clac3.P2.DomainUtil
open Clac3.Testing.Testing
open Clac3.Data

let customRules: RewriteRule list = [
    //{
    //    pattern = Value (PAtom (Value (PVariable (Value "x"))))
    //    replacer = Args.zero (aInt 5)
    //}
    {
        pattern = vNC [vKw "fibonacci"; vInt 0]
        replacer = Args.zero (aInt 0)
    }
    {
        pattern = vNC [vKw "fibonacci"; vInt 1]
        replacer = Args.zero (aInt 1)
    }
    {
        pattern = vNC [vKw "fibonacci"; pInt]
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
            FAtom (FFloat 0.0),
            FBranch (
                FCall ("intEquals", [|FCall("x", [||]); FAtom(FInteger 1)|]),
                FAtom (FFloat 1.0),
                FCall ("addFloats", [|
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

let factorialFnFloat = {
    ident = "fibonacciFloat";
    lambda = Custom (
        [|"x"|],
        FBranch (
            FCall ("floatEquals", [|FCall("x", [||]); FAtom(FFloat 0.0)|]),
            FAtom (FFloat 0.0),
            FBranch (
                FCall ("floatEquals", [|FCall("x", [||]); FAtom(FFloat 1.0)|]),
                FAtom (FFloat 1.0),
                FCall ("addFloats", [|
                    FCall ("fibonacciFloat", [|
                        FCall ("subFloats", [|FCall("x", [||]); FAtom(FFloat 1.0)|])
                    |])
                    FCall ("fibonacciFloat", [|
                        FCall ("subFloats", [|FCall("x", [||]); FAtom(FFloat 2.0)|])
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

let appFunc2 = ExtendedFunctionalApplication([|factorialFnFloat|],
    [|FCall ("fibonacciFloat", [|faFl 25|])|]
)

let measureApp (app: Application<'a, 'b, 'c>) (toStr: 'c -> string) = 
    let args = app.getEvalArgs
    let result = Performance.measureTime (fun () -> app.eval args)
    printfn "Results: \n%s" (result |> fst |> Seq.map toStr |> String.concat "\n")
    printfn "Time: %ims" (result |> snd |> int)

let measureAppRepeatedly n (app: Application<'a, 'b, 'c>) (toStr: 'c -> string) = 
    let args = app.getEvalArgs
    let result = Performance.measureAverageTime n (fun () -> app.eval args)
    printfn "Results: \n%s" (result |> fst |> Seq.map toStr |> String.concat "\n")
    printfn "Time: %ims" (result |> snd |> int |> fun t -> t / n)

//measureAppRepeatedly 20 appRules ToString.expression
//measureAppRepeatedly 20 appFunc P2ToString.atom
//measureAppRepeatedly 20 appFunc2 P2ToString.atom

let ruleTestApp = RewriteRuleApplication(testRules, testExprs)
ruleTestApp.runProgram |> Seq.iter (fun (result) -> printfn "%s" (ToString.expression result))