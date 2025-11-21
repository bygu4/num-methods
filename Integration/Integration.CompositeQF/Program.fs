open Common
open Functions
open QuadratureForms
open Console
open Integration

open System

let printHeader () =
    printfn "
Приближённое вычисление интеграла по составным квадратурным формулам
Уточнение по Рунге-Ромбергу"

// ------------ Меню ------------

type FirstMenuAction =
    | StartComputation
    | ChangeFunction
    | ChangeIntervalBounds
    | ChangeNumberOfSections
    | ChangeQuadratureForm

type SecondMenuAction =
    | RefineByRunge
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
\tm --- изменить число промежутков деления
\tq --- выбрать другую КФ
"
    match readString "Введите действие: " with
    | "" 
    | "s" -> StartComputation
    | "f" -> ChangeFunction
    | "i" -> ChangeIntervalBounds
    | "m" -> ChangeNumberOfSections
    | "q" -> ChangeQuadratureForm
    | _ ->
        printfn "Неизвестное действие, попробуйте снова"
        readFirstMenuAction ()

[<TailCall>]
let rec readSecondMenuAction () =
    printfn "
Доступные действия:
\tr --- начать уточнение по Рунге-Ромбергу
\tf --- выбрать другую функцию
\tp --- заново выбрать параметры
\te --- выйти из программы
"
    match readString "Введите действие: " with
    | "" 
    | "r" -> RefineByRunge
    | "f" -> SelectNewFunction
    | "p" -> ChangeParameters
    | "e" -> ExitProgram
    | _ ->
        printfn "Неизвестное действие, попробуйте снова"
        readSecondMenuAction ()

let printMenuParams funcKind a b m quadForms =
    printf "\n"
    printfn "f(x) = %s" (testFuncStr funcKind)
    printfn "[a, b] = [%A, %A]" a b
    printfn "m = %d; h = %A" m (step a b m)
    printfn "%s" <| String.Join(", ", List.map quadratureFormStr quadForms)

[<TailCall>]
let rec openFirstMenu funcKind a b m quadForms =
    let f = testFunc funcKind 0
    let expectedRes = testFuncInt funcKind 0 a b

    let inline compute quadForm =
        let res = integrateComposite quadForm f a b m

        let absErr = abs (expectedRes - res)
        let relErr = abs (absErr / expectedRes)

        printfn "\nВычисление с помощью %s\n" (quadratureFormStr quadForm)
        printfn "Точное значение интеграла f(x): J = %.16g" expectedRes
        printfn "Приближенное значение интеграла f(x): J(h) = %.16g" res
        printfn "Абсолютная погрешность: |J - J(h)| = %.16g" absErr
        printfn "Относительная погрешность: |J - J(h)| / |J| = %.16g" relErr

        waitForAnyKey ()

    let rec openSecondMenu curM =
        printMenuParams funcKind a b curM quadForms

        match readSecondMenuAction () with
        | RefineByRunge ->
            let l = readStepsMultiplier ()

            printf "\n"
            printRefinedTable quadForms funcKind a b curM l

            waitForAnyKey ()
            openSecondMenu (curM * l)
        | SelectNewFunction ->
            let funcKind = readFunctionKind2 ()
            openFirstMenu funcKind a b m quadForms
        | ChangeParameters ->
            let a, b = readIntervalBounds ()
            let m = readNumberOfSections ()
            let quadForms = readQuadratureForms ()
            openFirstMenu funcKind a b m quadForms
        | ExitProgram -> exit 0

    printMenuParams funcKind a b m quadForms

    match readFirstMenuAction () with
    | StartComputation ->
        quadForms |> List.iter compute
        openSecondMenu m
    | ChangeFunction ->
        let funcKind = readFunctionKind2 ()
        openFirstMenu funcKind a b m quadForms
    | ChangeIntervalBounds ->
        let a, b = readIntervalBounds ()
        openFirstMenu funcKind a b m quadForms
    | ChangeNumberOfSections ->
        let m = readNumberOfSections ()
        openFirstMenu funcKind a b m quadForms
    | ChangeQuadratureForm ->
        let quadForms = readQuadratureForms ()
        openFirstMenu funcKind a b m quadForms

// ------------ Точка входа ------------

printHeader ()
let funcKind = readFunctionKind2 ()
let a, b = readIntervalBounds ()
let m = readNumberOfSections ()
let quadForms = readQuadratureForms ()
openFirstMenu funcKind a b m quadForms
