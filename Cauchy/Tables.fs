module Tables

open System

open Methods

// ------------ Решение тестовой задачи ------------

let f _x y = -y + y ** 2.0
let sln1 _x = 0.0
let sln2 c x = 1.0 / (1.0 + c * Math.E ** x)
let cauchySln x_0 y_0 =
    match y_0 with
    | 0.0 -> sln1
    | _ ->
        sln2 ((1.0 / y_0 - 1.0) / Math.E ** x_0)

// ------------ Вывод таблиц с результатами ------------

let printPointsTable x_0 y_0 h N =
    let printTableHeader () =
        printfn "i\tx_i\t\ty_i"
        printfn "%s" (String.replicate 36 "-")
    
    let printTableRow x_0 y_0 h i =
        let x_i = x_0 + h * float i
        let y_i = cauchySln x_0 y_0 x_i
        printfn "%d\t%-12g\t%-12g" i x_i y_i 

    printTableHeader ()
    for i in seq { 0 .. N } do
        printTableRow x_0 y_0 h i

let printResultsTable methods x_0 y_0 h N =
    let printTableHeader () =
        printfn "Название метода\t\ty_T\t\ty_N\t\t|y_T - y_N|\t|y_T - y_N| / |y_T|"
        printfn "%s" (String.replicate 91 "-")

    let printTableRow method x_0 y_0 h N =
        let y_T = cauchySln x_0 y_0 (x_0 + h * float N)
        let y_N = compute method true f x_0 y_0 h N

        let absErr = abs (y_T - y_N)
        let relErr = abs (absErr / y_T)

        printfn
            "%-17s\t%-12g\t%-12g\t%-12g\t%-12g"
            (numMethodStr method)
            y_T
            y_N
            absErr
            relErr

    printTableHeader ()
    for method in methods do
        printTableRow method x_0 y_0 h N

let printRefinedTable methods x_0 y_0 h N l =
    let printTableHeader () =
        printfn "Название метода\t\tJ\t\tJ(h)\t\t|J - J(h)|\tJ(h/l)\t\t|J - J(h/l)|\tJ_R\t\t|J - J_R|"
        printfn "%s" (String.replicate 131 "-")

    let printTableRow method f x_0 y_0 h N l =
        let J = cauchySln x_0 y_0 (x_0 + h * float N)

        let J_h = compute method true f x_0 y_0 h N
        let J_hl = compute method true f x_0 y_0 (h / float l) (N * l)
        let J_R = refineByRunge method J_h J_hl l

        let J_h_err = abs (J - J_h)
        let J_hl_err = abs (J - J_hl)
        let J_R_err = abs (J - J_R)

        printfn
            "%-17s\t%-12g\t%-12g\t%-12g\t%-12g\t%-12g\t%-12g\t%-12g"
            (numMethodStr method)
            J
            J_h
            J_h_err
            J_hl
            J_hl_err
            J_R
            J_R_err

    printTableHeader ()
    for method in methods do
        printTableRow method f x_0 y_0 h N l
