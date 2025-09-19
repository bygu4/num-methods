open Interpolation
open Console

let printHeader () =
    printfn "
Задача алгебраического интерполирования

Вариант 4
f(x) = sqrt(1 + x^2)
"

let f x = sqrt (1.0 + x ** 2.0)

// ------------ Меню ------------

type FirstMenuAction =
    | ExitProgram
    | ReadSectionBounds
    | ReadNumberOfPoints
    | ReadPointType
    | GeneratePoints

type SecondMenuAction =
    | ExitMenu
    | ReadPolynomialDegree
    | ReadInterpolationPoint
    | ReadInterpolationMethod
    | StartInterpolation

[<TailCall>]
let rec readFirstMenuAction () =
    printfn "
Доступные действия:
\t0 --- выйти из программы
\t1 --- выбрать отрезок интерполирования
\t2 --- выбрать число узлов интерполирования
\t3 --- выбрать характер узлов
\t4 --- сгенерировать узлы интерполирования
"
    match readInt "Выберите действие: " with
    | 0 -> ExitProgram
    | 1 -> ReadSectionBounds
    | 2 -> ReadNumberOfPoints
    | 3 -> ReadPointType
    | 4 -> GeneratePoints
    | _ ->
        printfn "Неизвестное действие, попробуйте снова"
        readFirstMenuAction ()

[<TailCall>]
let rec readSecondMenuAction () =
    printfn "
Доступные действия:
\t0 --- перейти назад
\t1 --- выбрать степень интерполяционного многочлена
\t2 --- выбрать точку интерполирования
\t3 --- выбрать метод интерполирования
\t4 --- начать интерполяцию
"
    match readInt "Выберите действие: " with
    | 0 -> ExitMenu
    | 1 -> ReadPolynomialDegree
    | 2 -> ReadInterpolationPoint
    | 3 -> ReadInterpolationMethod
    | 4 -> StartInterpolation
    | _ ->
        printfn "Неизвестное действие, попробуйте снова"
        readSecondMenuAction ()

let printFirstMenuParams A B m pointType =
    printfn "
[A, B] = [%A, %A]
m = %d
Используются %s узлы" A B m (pointTypeToStr pointType)

let printSecondMenuParams n x method =
    printfn "
n = %d
x = %A
Используется %s" n x (methodToStr method)

[<TailCall>]
let rec openFirstMenu f A B m pointType =
    printFirstMenuParams A B m pointType

    let rec openSecondMenu f A B m pointType points n x method =
        printSecondMenuParams n x method

        match readSecondMenuAction () with
        | ExitMenu -> openFirstMenu f A B m pointType
        | ReadPolynomialDegree ->
            let n = readPolynomialDegree m
            openSecondMenu f A B m pointType points n x method
        | ReadInterpolationPoint ->
            let x = readInterpolationPoint ()
            openSecondMenu f A B m pointType points n x method
        | ReadInterpolationMethod ->
            let method = readInterpolationMethod ()
            openSecondMenu f A B m pointType points n x method
        | StartInterpolation ->
            let sorted = sortFromPoint x points
            printfn "\nБлижайшие к x точки:\n"
            printSortedTable f sorted x
            waitForAnyKey ()

            let filtered = List.take (n + 1) sorted
            printfn "Взяты первые %d точек" (n + 1)

            let res = methodToFunc method false f filtered x
            printfn "Вычисленное значение в точке: P_n(x) = %A" res
            printfn "Абсолютная погрешность: |f(x) - P_n(x)| = %A" (f x - res)
            waitForAnyKey ()
            openSecondMenu f A B m pointType points n x method

    match readFirstMenuAction () with
    | ExitProgram -> exit 0
    | ReadSectionBounds ->
        let A, B = readSectionBounds ()
        openFirstMenu f A B m pointType
    | ReadNumberOfPoints ->
        let m = readNumberOfPoints ()
        openFirstMenu f A B m pointType
    | ReadPointType ->
        let pointType = readPointType ()
        openFirstMenu f A B m pointType
    | GeneratePoints ->
        let points = generatePoints pointType A B m
        printfn "\nСгенерированы %d точек:\n" m
        printPointsTable f points
        waitForAnyKey ()

        let n = readPolynomialDegree m
        let x = readInterpolationPoint ()
        let method = readInterpolationMethod ()
        openSecondMenu f A B m pointType points n x method

// ------------ Точка входа ------------

printHeader ()
let A, B = readSectionBounds ()
let m = readNumberOfPoints ()
let pointType = readPointType ()
openFirstMenu f A B m pointType
