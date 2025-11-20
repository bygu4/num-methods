open Common
open Functions
open Integration
open Console

let printHeader =
    printfn "
Численное интегрирование 1

Вариант 4
ρ(x) = x^(-1/4)"

// ------------ Меню ------------

type FirstMenuAction =
    | StartComputation
    | ChangeFunction
    | ChangeIntervalBounds
    | ChangeNumberOfPoints
    | ChangePoints

type SecondMenuAction =
    | SelectNewFunction
    | ChangeParameters
    | ExitProgram

[<TailCall>]
let rec readFirstMenuAction () =
    printfn "
Доступные действия:
\ts --- перейти к построению ИКФ
\tf --- выбрать другую функцию
\ti --- изменить границы промежутка интегрирования
\tn --- изменить число точек
\tx --- заново выбрать точки
"
    match readString "Введите действие: " with
    | "" 
    | "s" -> StartComputation
    | "f" -> ChangeFunction
    | "i" -> ChangeIntervalBounds
    | "n" -> ChangeNumberOfPoints
    | "x" -> ChangePoints
    | _ ->
        printfn "Неизвестное действие, попробуйте снова"
        readFirstMenuAction ()

[<TailCall>]
let rec readSecondMenuAction () =
    printfn "
Доступные действия:
\tf --- выбрать другую функцию
\tp --- заново выбрать параметры
\te --- выйти из программы
"
    match readString "Введите действие: " with
    | "" 
    | "f" -> SelectNewFunction
    | "p" -> ChangeParameters
    | "e" -> ExitProgram
    | _ ->
        printfn "Неизвестное действие, попробуйте снова"
        readSecondMenuAction ()

[<TailCall>]
let rec openFirstMenu funcKind a b N points =
    let f = testFunc (N - 1) funcKind
    let expectedRes = testFuncWeighedInt (N - 1) a b funcKind

    let inline openSecondMenu () =
        match readSecondMenuAction () with
        | SelectNewFunction ->
            let funcKind = readFunctionKind ()
            openFirstMenu funcKind a b N points
        | ChangeParameters ->
            let a, b = readIntervalBounds ()
            let N = readNumberOfPoints ()
            let points = readPoints N
            openFirstMenu funcKind a b N points
        | ExitProgram -> exit 0

    match readFirstMenuAction () with
    | StartComputation ->
        let res = integrateIQF false f a b points
        checkErrorIQF false a b points |> ignore

        let absErr = abs (expectedRes - res)
        let relErr = abs (absErr / expectedRes)

        printf "\n"
        printfn "Точное значение интеграла ρ(x)f(x): %.16g" expectedRes
        printfn "Приближенное значение интеграла ρ(x)f(x): %.16g" res
        printfn "Абсолютная погрешность: %.16g" absErr
        printfn "Относительная погрешность: %.16g" relErr

        waitForAnyKey ()
        openSecondMenu ()
    | ChangeFunction ->
        let funcKind = readFunctionKind ()
        openFirstMenu funcKind a b N points
    | ChangeIntervalBounds ->
        let a, b = readIntervalBounds ()
        openFirstMenu funcKind a b N points
    | ChangeNumberOfPoints ->
        let N = readNumberOfPoints ()
        openFirstMenu funcKind a b N points
    | ChangePoints ->
        let points = readPoints N
        openFirstMenu funcKind a b N points

// ------------ Точка входа ------------

let funcKind = readFunctionKind ()
let a, b = readIntervalBounds ()
let N = readNumberOfPoints ()
let points = readPoints N
openFirstMenu funcKind a b N points
