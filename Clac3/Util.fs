module Clac3.Util

module Option =
    let rec private combineInner (acc: 'a list option) (options: 'a option list) =
        if options.Length = 0 then acc |> Option.map List.rev else

        match options[0], acc with
        | Some v, Some vAcc -> combineInner (Some (v::vAcc)) options[1..]
        | _ -> None
       
    let combine options = combineInner (Some []) options

    let tupleWith v option = option |> Option.map (fun optionV -> v,optionV)