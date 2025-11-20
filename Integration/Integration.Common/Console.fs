module Console

open Common
open Functions
open QuadratureForms

// ------------ Пользовательский ввод ------------

[<TailCall>]
let rec readFunctionKind () =
    printfn $"
Доступные функции:
\t0 --- {testFuncStr Constant}
\t1 --- {testFuncStr FirstDegreePolynomial}
\t2 --- {testFuncStr SecondDegreePolynomial}
\t3 --- {testFuncStr ThirdDegreePolynomial}
\t4 --- {testFuncStr NthDegreePolynomial}
\t5 --- {testFuncStr Trigonometric}
"
    match readInt "Выберите функцию: " with
    | 0 -> Constant
    | 1 -> FirstDegreePolynomial
    | 2 -> SecondDegreePolynomial
    | 3 -> ThirdDegreePolynomial
    | 4 -> NthDegreePolynomial
    | 5 -> Trigonometric
    | _ ->
        printfn "Неизвестная функция, попробуйте снова"
        readFunctionKind ()

[<TailCall>]
let rec readFunctionKind2 () =
    printfn $"
Доступные функции:
\t0 --- {testFuncStr Constant}
\t1 --- {testFuncStr FirstDegreePolynomial}
\t2 --- {testFuncStr SecondDegreePolynomial}
\t3 --- {testFuncStr ThirdDegreePolynomial}
\t4 --- {testFuncStr Trigonometric}
"
    match readInt "Выберите функцию: " with
    | 0 -> Constant
    | 1 -> FirstDegreePolynomial
    | 2 -> SecondDegreePolynomial
    | 3 -> ThirdDegreePolynomial
    | 4 -> Trigonometric
    | _ ->
        printfn "Неизвестная функция, попробуйте снова"
        readFunctionKind ()

[<TailCall>]
let rec readQuadratureForm () =
    printfn $"
Доступные КФ:
\t0 --- использовать все КФ
\t1 --- {quadratureFormStr RectangleLeft}
\t2 --- {quadratureFormStr RectangleRight}
\t3 --- {quadratureFormStr RectangleMiddle}
\t4 --- {quadratureFormStr Trapezoid}
\t5 --- {quadratureFormStr Simpson}
"
    match readString "Выберите КФ: " with
    | ""
    | "0" -> None
    | "1" -> Some RectangleLeft
    | "2" -> Some RectangleRight
    | "3" -> Some RectangleMiddle
    | "4" -> Some Trapezoid
    | "5" -> Some Simpson
    | _ ->
        printfn "Неизвестная КФ, попробуйте снова"
        readQuadratureForm ()

[<TailCall>]
let rec readIntervalBounds () =
    let a = readDouble "Введите левую границу промежутка интегрирования (a): "
    let b = readDouble "Введите правую границу промежутка интегрирования (b > a): "

    if a < b then a, b
    else

    printfn "Требуется a < b, попробуйте снова"
    readIntervalBounds ()

[<TailCall>]
let rec readNumberOfPoints () =
    let N = readInt "Введите число узлов (N >= 1): "

    if N >= 1 then N
    else

    printfn "Требуется N >= 1, попробуйте снова"
    readNumberOfPoints ()

let readPoints N =
    let rec readPoints' n acc =
        match n with
        | n when n > 0 ->
            let x = readDouble $"x_{N - n + 1} = "

            if not <| List.contains x acc then readPoints' (n - 1) (x :: acc)
            else

            printfn "Узлы должны быть попарно различны, попробуйте снова"
            readPoints' n acc
        | _ -> acc
    in
    printfn "Введите %d попарно различных узлов:" N
    readPoints' N [] |> List.rev
