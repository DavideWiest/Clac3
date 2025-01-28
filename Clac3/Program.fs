
open Clac3.Type
open Clac3.Application
open Clac3.Expression
open Clac3.P1.PatternReplacer
open Clac3.P1.DomainUtil
open Clac3.P2.DomainUtil
open Clac3.App.P2Application
open Clac3.App.InterpretedApp
open Clac3.BuiltIn.P1.Core
open Clac3.Data

// defined in the functional domain
let fibFn = toBinding ("fib", [TInteger; TInteger],
    toCustomFn (["x"],
        toBranch (
            toReference ("ltInt", [|toValueReference "x"; faInt 2|]),
            toValueReference "x",
            toReference ("addInt", [|
                toReference ("fib", [|
                    toReference ("subtractInt", [|toValueReference "x"; faInt 1|])
                |])
                toReference ("fib", [|
                    toReference ("subtractInt", [|toValueReference "x"; faInt 2|])
                |])
            |])
        )
    )
)

let fibFnFloat = toBinding ("fibFloat", [TFloat; TFloat],
    toCustomFn (["x"],
        toBranch (
            toReference ("ltFloat", [|toValueReference "x"; faFl 2.0|]),
            toValueReference "x",
            toReference ("addFloat", [|
                toReference ("fibFloat", [|
                    toReference ("subtractFloat", [|toValueReference "x"; faFl 1.0|])
                |])
                toReference ("fibFloat", [|
                    toReference ("subtractFloat", [|toValueReference "x"; faFl 2.0|])
                |])
            |])
        )
    )
)

// defined in phase-1 domain
let factFn = toDefinition("fact", [TInteger; TInteger], 
    toRawCustomFn (["x"],
        Node [
            aKw "if"; Node [aVar "x"; aKw "="; aInt 0]; 
            aKw "then"; aInt 1; 
            aKw "else"; Node [
                aVar "x"; aKw "*"; Node [aKw "fact"; Node [aVar "x"; aKw "-"; aInt 1]]
            ]
        ]
    )
)

let factRule = {
    pattern = NC [pInt; vKw "!"]
    replacer = Args.one (fun n -> taNo [taKw "fact"; n])
}

// applications
let appFunc = ExtendedFunctionalApplication([|fibFn|],
    [|toReference ("fib", [|faInt 25|])|]
)

let appFunc2 = ExtendedFunctionalApplication([|fibFnFloat|],
    [|toReference ("fibFloat", [|faFl 25|])|]
)

let clacApp = ExtendedInterpretedApp(
    [factRule],
    [|fibFn|],
    [|factFn|],
    [Node [aKw "fib"; Node [aInt 4; aKw "!"]]]
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

printfn "Rules: %A" (coreRules)

measureApp clacApp (sprintf "%A")

