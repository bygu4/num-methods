module Console

open Common
open Methods

// ------------ Пользовательский ввод ------------

let readInitialParams () =
    let x_0 = readDouble "Введите начальную точку (x_0): "
    let y_0 = readDouble "Введите значение в начальной точке (y_0): "
    x_0, y_0

[<TailCall>]
let rec readStep () =
    let h = readDouble "Введите шаг (h > 0): "

    if h > 0 then h
    else

    printfn "Требуется h > 0, попробуйте снова"
    readStep ()

[<TailCall>]
let rec readNumberOfSteps () =
    let N = readInt "Введите число шагов (N >= 0): "

    if N >= 0 then N
    else

    printfn "Требуется N >= 0, попробуйте снова"
    readNumberOfSteps ()

[<TailCall>]
let rec readStepsMultiplier () =
    let l = readInt "Во сколько раз уменьшить шаг? (l >= 2): "

    if l >= 2 then l
    else

    printfn "Требуется l >= 2, попробуйте снова"
    readStepsMultiplier ()

[<TailCall>]
let rec readNumMethods () =
    printfn $"
Доступные КФ:
\t0 --- использовать все методы
\t1 --- {numMethodStr Euler}
\t2 --- {numMethodStr EulerI}
\t3 --- {numMethodStr EulerII}
\t4 --- {numMethodStr RungeKutta}
"
    match readString "Выберите метод: " with
    | ""
    | "0" -> [
        Euler
        EulerI
        EulerII
        RungeKutta ]
    | "1" -> [ Euler ]
    | "2" -> [ EulerI ]
    | "3" -> [ EulerII ]
    | "4" -> [ RungeKutta ]
    | _ ->
        printfn "Неизвестный метод, попробуйте снова"
        readNumMethods ()
