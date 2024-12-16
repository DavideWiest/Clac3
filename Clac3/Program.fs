
open Clac3.Type
open Clac3.Application
open Clac3.Expression
open Clac3.P1.DomainUtil
open Clac3.P2.Application
open Clac3.P2.DomainUtil
open Clac3.ClacApplication
open Clac3.Data

let fibFn = toBinding ("fib", [TInteger; TInteger],
    toCustomFn (
        ["x"],
        toBranch (
            toReference ("eqInt", [|toValueReference "x"; faInt 0|]),
            faInt 0,
            toBranch (
                toReference ("eqInt", [|toValueReference "x"; faInt 1|]),
                faInt 1,
                toReference ("addInt", [|
                    toReference ("fibonacci", [|
                        toReference ("subtractInt", [|toValueReference "x"; faInt 1|])
                    |])
                    toReference ("fibonacci", [|
                        toReference ("subtractInt", [|toValueReference "x"; faInt 2|])
                    |])
                |])
            )
        )
    )
)

let fibFnFloat = toBinding("fibFloat", [TFloat; TFloat],
    toCustomFn (
        ["x"],
        toBranch (
            toReference ("eqFloat", [|toValueReference "x"; faFl 0.0|]),
            faFl 0.0,
            toBranch (
                toReference ("eqFloat", [|toValueReference "x"; faFl 1.0|]),
                faFl 1.0,
                toReference ("addFloat", [|
                    toReference ("fibonacciFloat", [|
                        toReference ("subtractFloat", [|toValueReference "x"; faFl 1.0|])
                    |])
                    toReference ("fibonacciFloat", [|
                        toReference ("subtractFloat", [|toValueReference "x"; faFl 2.0|])
                    |])
                |])
            )
        )
    )
)


let appFunc = ExtendedFunctionalApplication([|fibFn|],
    [|toReference ("fib", [|faInt 25|])|]
)

let appFunc2 = ExtendedFunctionalApplication([|fibFnFloat|],
    [|toReference ("fibFloat", [|faFl 25|])|]
)

let clacApp = ExtendedClacApplication(
    [],
    [|fibFn|],
    [Node [aKw "fib"; aInt 25]]
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

measureApp clacApp (sprintf "%A")