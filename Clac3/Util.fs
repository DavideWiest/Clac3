﻿module Clac3.Util

module Option =
    let rec private combineInner (acc: 'a list option) (options: 'a option list) =
        if options.Length = 0 then acc |> Option.map List.rev else

        match options[0], acc with
        | Some v, Some vAcc -> combineInner (Some (v::vAcc)) options[1..]
        | _ -> None
       
    let combine options = combineInner (Some []) options

    let tupleWith v option = option |> Option.map (fun optionV -> v,optionV)
    let tupleWithRev option v = option |> Option.map (fun optionV -> optionV,v)

    let reverseTuple (t: ('a * 'b) option) = 
        t |> Option.map (fun (a,b) -> b,a)

module List = 
    let headOption = function
        | [] -> None
        | h::_ -> Some h

    let toOption = function
        | [] -> None
        | l -> Some l

module ToString =
    let inParans s = "(" + s + ")"
    let inBrackets s = "[" + s + "]"

module Error = 
    let typeError expectedType item = failwithf "Expected %A, got %A" expectedType item
    let typeErrorWithCtx expectedType item ctx = failwithf "Expected %A, got %A: %A" expectedType item ctx