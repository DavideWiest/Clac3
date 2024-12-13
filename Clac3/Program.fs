
open Clac3.Application
open Clac3.FExpression
open Clac3.Function
open Clac3.P2.Application
open Clac3.P2.DomainUtil
open Clac3.Testing.Testing
open Clac3.Data

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

//measureAppRepeatedly 20 appFunc P2ToString.atom
//measureAppRepeatedly 100 appFunc2 P2ToString.atom

runTestsPhase1 ()