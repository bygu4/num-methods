module Console

open System
open Methods

// ------------ Пользовательский ввод ------------

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

[<TailCall>]
let rec readSectionBounds () =
    let A = readDouble "Введите левую границу отрезка (A): "
    let B = readDouble "Введите правую границу отрезка (B): "

    if A < B then A, B
    else

    printfn "Требуется A < B, попробуйте снова"
    readSectionBounds ()

[<TailCall>]
let rec readNumberOfSections () =
    let N = readInt "Введите число отрезков табуляции (N): "

    if N >= 2 then N
    else

    printfn "Требуется N >= 2, попробуйте снова"
    readNumberOfSections ()

[<TailCall>]
let rec readEpsilon () =
    let epsilon = readDouble "Введите точность вычисления (epsilon): "

    if epsilon > 0 then epsilon
    else

    printfn "Требуется epsilon > 0, попробуйте снова"
    readEpsilon ()

[<TailCall>]
let rec readTargetSection sections =
    let i = readInt "Введите номер целевого отрезка: "

    if List.contains i sections then i
    else

    printfn "На данном отрезке не было найдено корней, попробуйте снова"
    readTargetSection sections

[<TailCall>]
let rec readMethod () =
    printfn $"
Доступные методы:
\t0 --- {bisectMethodStr}
\t1 --- {newtonMethodStr}
\t2 --- {newtonModMethodStr}
\t3 --- {secantMethodStr}
"
    match readInt "Введите номер метода: " with
    | 0 -> Bisect
    | 1 -> Newton
    | 2 -> NewtonMod
    | 3 -> Secant
    | _ ->
        printfn "Неизвестный метод, попробуйте снова"
        readMethod ()
