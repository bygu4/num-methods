module Console

open Functions
open Console

// ------------ Пользовательский ввод ------------

[<TailCall>]
let rec readFunctionKind () =
    printfn $"
Доступные функции:
\t1 --- {testFuncStr SecondDegreePolynomial}
\t2 --- {testFuncStr ThirdDegreePolynomial}
\t3 --- {testFuncStr Exponent}
\t4 --- {testFuncStr Trigonometric}
"
    match readInt "Выберите функцию: " with
    | 1 -> SecondDegreePolynomial
    | 2 -> ThirdDegreePolynomial
    | 3 -> Exponent
    | 4 -> Trigonometric
    | _ ->
        printfn "Неизвестная функция, попробуйте снова"
        readFunctionKind ()

[<TailCall>]
let rec readNumberOfPoints () =
    let m = readInt "Введите число точек (m): "

    if m >= 5 then m
    else

    printfn "Требуется m >= 5, попробуйте снова"
    readNumberOfPoints ()

let rec readStartingPoint () =
    readDouble "Введите начальную точку (x_0): "

[<TailCall>]
let rec readStep () =
    let h = readDouble "Введите шаг (h): "

    if h > 0 then h
    else

    printfn "Требуется h > 0, попробуйте снова"
    readStep ()
