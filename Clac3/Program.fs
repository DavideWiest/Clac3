
open Clac3.Expression
open Clac3.FunctionalExpression
open Clac3.Type
open Clac3.Function
open Clac3.Application
open Clac3.P2.Application
open Clac3.P2.DomainUtil
open Clac3.Testing.Testing
open Clac3.Data

let factorialFn = {
    ident = "fibonacci";
    signature = {
        args = [TInteger];
        returnType = TInteger
    };
    binding = Custom (
        [|"x"|],
        toBranch (
            toFCall ("intEquals", [|toFCall("x", [||]); FAtom(Integer 0)|]),
            FAtom (Float 0.0),
            toBranch (
                toFCall ("intEquals", [|toFCall("x", [||]); FAtom(Integer 1)|]),
                FAtom (Float 1.0),
                toFCall ("addFloats", [|
                    toFCall ("fibonacci", [|
                        toFCall ("subInts", [|toFCall("x", [||]); FAtom(Integer 1)|])
                    |])
                    toFCall ("fibonacci", [|
                        toFCall ("subInts", [|toFCall("x", [||]); FAtom(Integer 2)|])
                    |])
                |])
            )
        )
    )
}

let factorialFnFloat = {
    ident = "fibonacciFloat";
    signature = {
        args = [TFloat];
        returnType = TFloat
    };
    binding = Custom (
        [|"x"|],
        toBranch (
            toFCall ("floatEquals", [|toFCall("x", [||]); FAtom(Float 0.0)|]),
            FAtom (Float 0.0),
            toBranch (
                toFCall ("floatEquals", [|toFCall("x", [||]); FAtom(Float 1.0)|]),
                FAtom (Float 1.0),
                toFCall ("addFloats", [|
                    toFCall ("fibonacciFloat", [|
                        toFCall ("subFloats", [|toFCall("x", [||]); FAtom(Float 1.0)|])
                    |])
                    toFCall ("fibonacciFloat", [|
                        toFCall ("subFloats", [|toFCall("x", [||]); FAtom(Float 2.0)|])
                    |])
                |])
            )
        )
    )
}

let appFunc = ExtendedFunctionalApplication([|factorialFn|],
    [|toFCall ("fibonacci", [|faInt 25|])|]
)

let appFunc2 = ExtendedFunctionalApplication([|factorialFnFloat|],
    [|toFCall ("fibonacciFloat", [|faFl 25|])|]
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