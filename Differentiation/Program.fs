open Functions
open Differentiation
open Console

let printHeader () =
    printfn "Численное дифференцирование"

// ------------ Меню ------------

type FirstMenuAction =
    | StartComputation
    | ChangeFunction
    | ChangeNumberOfPoints
    | ChangeStartingPoint
    | ChangeStep

type SecondMenuAction =
    | SelectNewFunction
    | CreateNewTable
    | ExitProgram

[<TailCall>]
let rec readFirstMenuAction () =
    printfn "
Доступные действия:
\ts --- перейти к вычислению
\tf --- выбрать другую функцию
\tm --- изменить число точек
\tx --- изменить начальную точку
\th --- изменить шаг
"
    match readString "Введите действие: " with
    | "" 
    | "s" -> StartComputation
    | "f" -> ChangeFunction
    | "m" -> ChangeNumberOfPoints
    | "x" -> ChangeStartingPoint
    | "h" -> ChangeStep
    | _ ->
        printfn "Неизвестное действие, попробуйте снова"
        readFirstMenuAction ()

[<TailCall>]
let rec readSecondMenuAction () =
    printfn "
Доступные действия:
\tf --- выбрать другую функцию
\tt --- построить новую таблицу
\te --- выйти из программы
"
    match readString "Введите действие: " with
    | "" 
    | "f" -> SelectNewFunction
    | "t" -> CreateNewTable
    | "e" -> ExitProgram
    | _ ->
        printfn "Неизвестное действие, попробуйте снова"
        readSecondMenuAction ()

[<TailCall>]
let rec openFirstMenu funcKind m x_0 h =
    let f = testFunc funcKind
    let f' = testFunc' funcKind
    let f'' = testFunc'' funcKind
    let points = generatePoints m x_0 h

    let inline openSecondMenu () =
        printf "\n"
        printResultTable f f' f'' h points
        waitForAnyKey ()

        match readSecondMenuAction () with
        | SelectNewFunction ->
            let funcKind = readFunctionKind ()
            openFirstMenu funcKind m x_0 h
        | CreateNewTable ->
            let m = readNumberOfPoints ()
            let x_0 = readStartingPoint ()
            let h = readStep ()
            openFirstMenu funcKind m x_0 h
        | ExitProgram -> exit 0

    printf "\n"
    printPrepTable f points
    waitForAnyKey ()

    match readFirstMenuAction () with
    | StartComputation -> openSecondMenu ()
    | ChangeFunction ->
        let funcKind = readFunctionKind ()
        openFirstMenu funcKind m x_0 h
    | ChangeNumberOfPoints ->
        let m = readNumberOfPoints ()
        openFirstMenu funcKind m x_0 h
    | ChangeStartingPoint ->
        let x_0 = readStartingPoint ()
        openFirstMenu funcKind m x_0 h
    | ChangeStep ->
        let h = readStep ()
        openFirstMenu funcKind m x_0 h

// ------------ Точка входа ------------

printHeader ()
let funcKind = readFunctionKind ()
let m = readNumberOfPoints ()
let x_0 = readStartingPoint ()
let h = readStep ()
openFirstMenu funcKind m x_0 h
