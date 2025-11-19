module Console

open Common
open Interpolation

// ------------ Пользовательский ввод ------------

[<TailCall>]
let rec readSectionBounds () =
    let A = readDouble "Введите левую границу отрезка (A): "
    let B = readDouble "Введите правую границу отрезка (B > A): "

    if A < B then A, B
    else

    printfn "Требуется A < B, попробуйте снова"
    readSectionBounds ()

[<TailCall>]
let rec readNumberOfPoints () =
    let m = readInt "Введите число узлов интерполирования (m >= 1): "

    if m >= 1 then m
    else

    printfn "Требуется m >= 1, попробуйте снова"
    readNumberOfPoints ()

[<TailCall>]
let rec readPolynomialDegree m =
    let n = readInt "Введите степень интерполяционного многочлена (0 <= n <= m - 1): "

    if n >= 0 && n <= m - 1 then n
    else

    printfn "Требуется 0 <= n <= m - 1, попробуйте снова"
    readPolynomialDegree m

let readInterpolationPoint () =
    readDouble "Введите точку интерполирования (x): "

[<TailCall>]
let rec readPointType () =
    printfn $"
Доступные типы узлов интерполирования:
\t0 --- {equidistantPointStr}
\t1 --- {randomPointStr}
"
    match readInt "Выберите тип узлов интерполирования: " with
    | 0 -> Equidistant
    | 1 -> Random
    | _ ->
        printfn "Неизвестный тип узлов, попробуйте снова"
        readPointType ()

[<TailCall>]
let rec readInterpolationMethod () =
    printfn $"
Доступные методы интерполирования:
\t0 --- {lagrangeMethodStr}
\t1 --- {newtonMethodStr}
"
    match readInt "Введите номер метода интерполирования: " with
    | 0 -> Lagrange
    | 1 -> Newton
    | _ ->
        printfn "Неизвестный метод интерполирования, попробуйте снова"
        readInterpolationMethod ()
