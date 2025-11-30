open Common
open Methods
open Tables
open Console

open System

let printHeader () =
    printfn "
Численное решение Задачи Коши для обыкновенного
дифференциального уравнения первого порядка

Вариант 4
y' = -y + y^2,\ty(0) = 1/2
"

// ------------ Меню ------------

type FirstMenuAction =
    | StartComputation
    | ChangeInitialParams
    | ChangeStep
    | ChangeNumberOfSteps
    | ChangeMethod

type SecondMenuAction =
    | RefineByRunge
    | ChangeParameters
    | ExitProgram

[<TailCall>]
let rec readFirstMenuAction () =
    printfn "
Доступные действия:
\ts --- перейти к вычислению
\tx --- изменить начальные параметры
\th --- изменить шаг
\tn --- изменить число шагов
\tm --- выбрать другой метод
"
    match readString "Введите действие: " with
    | "" 
    | "s" -> StartComputation
    | "x" -> ChangeInitialParams
    | "h" -> ChangeStep
    | "n" -> ChangeNumberOfSteps
    | "m" -> ChangeMethod
    | _ ->
        printfn "Неизвестное действие, попробуйте снова"
        readFirstMenuAction ()

[<TailCall>]
let rec readSecondMenuAction () =
    printfn "
Доступные действия:
\tr --- начать уточнение по Рунге-Ромбергу
\tp --- заново выбрать параметры
\te --- выйти из программы
"
    match readString "Введите действие: " with
    | "" 
    | "r" -> RefineByRunge
    | "p" -> ChangeParameters
    | "e" -> ExitProgram
    | _ ->
        printfn "Неизвестное действие, попробуйте снова"
        readSecondMenuAction ()

let printMenuParams x_0 y_0 h N methods =
    printfn "y(%A) = %A" x_0 y_0
    printfn "h = %A; N = %d" h N
    printfn "%s" <| String.Join(", ", List.map numMethodStr methods)

[<TailCall>]
let rec openFirstMenu x_0 y_0 h N methods =

    let rec openSecondMenu curH curN =
        printMenuParams x_0 y_0 curH curN methods

        match readSecondMenuAction () with
        | RefineByRunge ->
            let l = readStepsMultiplier ()

            printf "\n"
            printRefinedTable methods x_0 y_0 curH curN l
            waitForAnyKey ()

            openSecondMenu (curH / float l) (curN * l)
        | ChangeParameters ->
            let x_0, y_0 = readInitialParams ()
            let h = readStep ()
            let N = readNumberOfSteps ()
            let methods = readNumMethods ()
            openFirstMenu x_0 y_0 h N methods
        | ExitProgram -> exit 0

    printf "\n"
    printPointsTable x_0 y_0 h N
    waitForAnyKey ()

    match readFirstMenuAction () with
    | StartComputation ->
        printf "\n"
        for method in methods do
            printfn "Вычисление через %s" (numMethodStr method)
            compute method false f x_0 y_0 h N |> ignore
            waitForAnyKey ()

        printResultsTable methods x_0 y_0 h N
        waitForAnyKey ()

        openSecondMenu h N
    | ChangeInitialParams ->
        let x_0, y_0 = readInitialParams ()
        openFirstMenu x_0 y_0 h N methods
    | ChangeStep ->
        let h = readStep ()
        openFirstMenu x_0 y_0 h N methods
    | ChangeNumberOfSteps ->
        let N = readNumberOfSteps ()
        openFirstMenu x_0 y_0 h N methods
    | ChangeMethod ->
        let methods = readNumMethods ()
        openFirstMenu x_0 y_0 h N methods

// ------------ Точка входа ------------

printHeader ()
let x_0, y_0 = readInitialParams ()
let h = readStep ()
let N = readNumberOfSteps ()
let methods = readNumMethods ()
openFirstMenu x_0 y_0 h N methods
