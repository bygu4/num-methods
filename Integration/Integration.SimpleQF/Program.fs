open Common
open Functions
open QuadratureForms
open Console

let printHeader =
    printfn "
Численное интегрирование 2"

// ------------ Меню ------------

type FirstMenuAction =
    | StartComputation
    | ChangeFunction
    | ChangeIntervalBounds
    | ChangeQuadratureForm

type SecondMenuAction =
    | SelectNewFunction
    | ChangeParameters
    | ExitProgram

[<TailCall>]
let rec readFirstMenuAction () =
    printfn "
Доступные действия:
\ts --- перейти к вычислению
\tf --- выбрать другую функцию
\ti --- изменить границы промежутка интегрирования
\tq --- выбрать другую КФ
"
    match readString "Введите действие: " with
    | "" 
    | "s" -> StartComputation
    | "f" -> ChangeFunction
    | "i" -> ChangeIntervalBounds
    | "q" -> ChangeQuadratureForm
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
let rec openFirstMenu funcKind a b quadForm =
    let f = testFunc 0 funcKind
    let expectedRes = testFuncInt 0 a b funcKind

    let inline compute quadForm =
        let res = quadratureFormIntegrate quadForm f a b

        let absErr = abs (expectedRes - res)
        let relErr = abs (absErr / expectedRes)

        printfn "\nВычисление с помощью %s\n" (quadratureFormStr quadForm)
        printfn "Точное значение интеграла f(x): %.16g" expectedRes
        printfn "Приближенное значение интеграла f(x): %.16g" res
        printfn "Абсолютная погрешность: %.16g" absErr
        printfn "Относительная погрешность: %.16g" relErr

        waitForAnyKey ()

    let inline openSecondMenu () =
        match readSecondMenuAction () with
        | SelectNewFunction ->
            let funcKind = readFunctionKind2 ()
            openFirstMenu funcKind a b quadForm
        | ChangeParameters ->
            let a, b = readIntervalBounds ()
            let quadForm = readQuadratureForm ()
            openFirstMenu funcKind a b quadForm
        | ExitProgram -> exit 0

    match readFirstMenuAction () with
    | StartComputation ->
        match quadForm with
        | Some quadForm -> compute quadForm
        | None ->
            [   RectangleLeft
                RectangleRight
                RectangleMiddle
                Trapezoid
                Simpson
            ] |> List.iter compute
        openSecondMenu ()
    | ChangeFunction ->
        let funcKind = readFunctionKind2 ()
        openFirstMenu funcKind a b quadForm
    | ChangeIntervalBounds ->
        let a, b = readIntervalBounds ()
        openFirstMenu funcKind a b quadForm
    | ChangeQuadratureForm ->
        let quadForm = readQuadratureForm ()
        openFirstMenu funcKind a b quadForm

// ------------ Точка входа ------------

let funcKind = readFunctionKind2 ()
let a, b = readIntervalBounds ()
let quadForm = readQuadratureForm ()
openFirstMenu funcKind a b quadForm
