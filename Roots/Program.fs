﻿open Methods
open Console

let printHeader () =
    printfn "
Методы решения нелинейного уравнения

Вариант 4
f(x) = sqrt(4x + 7) - 3cos(x)
"

let f x = sqrt (4.0 * x + 7.0) - 3.0 * cos x
let f' x = 2.0 / sqrt (4.0 * x + 7.0) + 3.0 * sin x
let f'' x = -4.0 / (4.0 * x + 7.0) ** (3.0 / 2.0) + 3.0 * cos x

// ------------ Меню ------------

type FirstMenuAction =
    | ExitProgram
    | ReadSectionBounds
    | ReadNumberOfSections
    | ReadEpsilon
    | StartRootSeparation

type SecondMenuAction =
    | ExitMenu
    | ReadTargetSection
    | ReadMethod
    | StartComputation

[<TailCall>]
let rec readFirstMenuAction () =
    printfn "
Доступные действия:
\t0 --- выйти из программы
\t1 --- выбрать границы отрезка
\t2 --- выбрать число отрезков табуляции
\t3 --- выбрать точность вычисления
\t4 --- начать отделение корней
"
    match readInt "Выберите действие: " with
    | 0 -> ExitProgram
    | 1 -> ReadSectionBounds
    | 2 -> ReadNumberOfSections
    | 3 -> ReadEpsilon
    | 4 -> StartRootSeparation
    | _ ->
        printfn "Неизвестное действие, попробуйте снова"
        readFirstMenuAction ()

[<TailCall>]
let rec readSecondMenuAction () =
    printfn "
Доступные действия:
\t0 --- перейти назад
\t1 --- выбрать целевой отрезок
\t2 --- выбрать метод уточнения корня
\t3 --- начать уточнение корня
"
    match readInt "Выберите действие: " with
    | 0 -> ExitMenu
    | 1 -> ReadTargetSection
    | 2 -> ReadMethod
    | 3 -> StartComputation
    | _ -> 
        printfn "Неизвестное действие, попробуйте снова"
        readSecondMenuAction ()

let printFirstMenuParams A B N epsilon =
    printfn "
[A, B] = [%A, %A]
N = %d
h = %A
epsilon = %A" A B N (step A B N) epsilon

let printSecondMenuParams A B N i method =
    printfn "
Уточнение на отрезке [%A, %A]
Используется %s" (left A B N i) (right A B N i) (methodToStr method)

[<TailCall>]
let rec openFirstMenu f f' A B N epsilon =
    printFirstMenuParams A B N epsilon

    let rec openSecondMenu f f' A B N epsilon sections i method =
        printSecondMenuParams A B N i method

        match readSecondMenuAction () with
        | ExitMenu -> openFirstMenu f f' A B N epsilon
        | ReadTargetSection ->
            let i = readTargetSection sections
            openSecondMenu f f' A B N epsilon sections i method
        | ReadMethod ->
            let method = readMethod ()
            openSecondMenu f f' A B N epsilon sections i method
        | StartComputation ->
            let left = left A B N i
            let right = right A B N i
            let root = methodToFunc method false f f' left right epsilon
            printfn "Найден корень требуемой точности: %A" root
            printfn "Абсолютная величина невязки: %A" (abs <| f root)
            waitForAnyKey ()
            openSecondMenu f f' A B N epsilon sections i method

    match readFirstMenuAction () with
    | ExitProgram -> exit 0
    | ReadSectionBounds ->
        let A, B = readSectionBounds ()
        openFirstMenu f f' A B N epsilon
    | ReadNumberOfSections ->
        let N = readNumberOfSections ()
        openFirstMenu f f' A B N epsilon
    | ReadEpsilon ->
        let epsilon = readEpsilon ()
        openFirstMenu f f' A B N epsilon
    | StartRootSeparation ->
        match separateRoots false f A B N with
        | [] ->
            printfn "Корней не найдено, попробуйте другие параметры"
            waitForAnyKey ()
            openFirstMenu f f' A B N epsilon
        | _ as sections ->
            waitForAnyKey ()
            let i = readTargetSection sections
            let method = readMethod ()
            openSecondMenu f f' A B N epsilon sections i method

// ------------ Точка входа ------------

printHeader ()
let A, B = readSectionBounds ()
let N = readNumberOfSections ()
let epsilon = readEpsilon ()
openFirstMenu f f' A B N epsilon
