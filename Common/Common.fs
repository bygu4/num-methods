module Common

open System

let log quiet fmt =
    Printf.kprintf (fun msg ->
        if not quiet then printfn "%s" msg
    ) fmt

let waitForAnyKey () =
    printf "\nНажмите любую клавишу, чтобы продолжить"
    ignore <| Console.ReadKey true
    Console.Write ("\r" + String.replicate Console.BufferWidth " " + "\r")

let readString msg =
    printf "%s" msg
    Console.ReadLine ()

[<TailCall>]
let rec readInt msg =
    printf "%s" msg
    let input = Console.ReadLine ()

    match Int32.TryParse input with
    | true, result -> result
    | false, _ ->
        printfn "Не удалось распознать число, попробуйте снова"
        readInt msg

[<TailCall>]
let rec readDouble msg =
    printf "%s" msg
    let input = Console.ReadLine ()

    match Double.TryParse input with
    | true, result -> result
    | false, _ ->
        printfn "Не удалось распознать число, попробуйте снова"
        readDouble msg
