module Differentiation

// ------------ Первая производная с точностью O(h^2) ------------

let private diffLeftH2 f h x =
    (-3.0 * f x + 4.0 * f (x + h) - f (x + 2.0 * h)) / (2.0 * h)

let private diffRightH2 f h x = diffLeftH2 f -h x

let private diffCenterH2 f h x =
    (f (x + h) - f (x - h)) / (2.0 * h)

let diffH2 f h points i =
    match List.length points with
    | len when len >= 3 ->
        match i with
        | 0 -> diffLeftH2 f h points.[i] |> Ok
        | x when x = len - 1 -> diffRightH2 f h points.[i] |> Ok
        | _ -> diffCenterH2 f h points.[i] |> Ok
    | _ ->
        Error $"Требуются хотя бы 3 точки, но получено: {List.length points}"

// ------------ Вторая производная с точностью O(h^2) ------------

let private diff2LeftH2 f h x =
    (2.0 * f x - 5.0 * f (x + h) + 4.0 * f (x + 2.0 * h) - f (x + 3.0 * h)) / h ** 2.0

let private diff2RightH2 f h x = diff2LeftH2 f -h x

let private diff2CenterH2 f h x =
    (f (x + h) - 2.0 * f x + f (x - h)) / h ** 2.0

let diff2H2 f h points i =
    match List.length points with
    | len when len >= 3 ->
        match i with
        | 0 -> diff2LeftH2 f h points.[i] |> Ok
        | x when x = len - 1 -> diff2RightH2 f h points.[i] |> Ok
        | _ -> diff2CenterH2 f h points.[i] |> Ok
    | _ ->
        Error $"Требуются хотя бы 3 точки, но получено: {List.length points}"

// ------------ Первая производная с точностью O(h^4) ------------

[<TailCall>]
let rec diffH4 f h points i =
    match List.length points with
    | len when len >= 5 ->
        match i with
        | 0 ->
            (-25.0 * f points.[0] + 48.0 * f points.[1]
            - 36.0 * f points.[2] + 16.0 * f points.[3]
            - 3.0 * f points.[4]) / (12.0 * h) |> Ok
        | 1 ->
            (-3.0 * f points.[0] - 10.0 * f points.[1]
            + 18.0 * f points.[2] - 6.0 * f points.[3]
            + f points.[4]) / (12.0 * h) |> Ok
        | x when x = len - 1 || x = len - 2 ->
            diffH4 f -h (List.rev points) (len - i - 1)
        | _ ->
            (f points.[i - 2] - 8.0 * f points.[i - 1]
            + 8.0 * f points.[i + 1] - f points.[i + 2]) / (12.0 * h) |> Ok
    | _ ->
        Error $"Требуются хотя бы 5 точек, но получено: {List.length points}"

// ------------ Генерация узлов ------------

let generatePoints m (x_0 : float) h =
    let mutable cur = x_0

    [
    for _ in seq { 1 .. m } do
        yield cur
        cur <- cur + h
    ]

// ------------ Вывод таблиц с результатами ------------

let printPrepTable f points =
    let printTableHeader () =
        printfn "i\tx_i\t\ty_i = f(x_i)"
        printfn "%s" (String.replicate 36 "-")
    
    let printTableRow f (points : float list) i =
        let x_i = points.[i]
        let y_i = f x_i
        printfn "%d\t%-12g\t%-12g" i x_i y_i

    printTableHeader ()
    for i in seq { 0 .. List.length points - 1 } do
        printTableRow f points i

let printResultTable f f' f'' h points =
    let printTableHeader () =
        printfn "i\tx_i\t\ty_i\t\tf'\t\t~f', O(h^2)\tError, O(h^2)\t~f', O(h^4)\tError, O(h^4)\tf''\t\t~f'', O(h^2)\tError, O(h^2)"
        printfn "%s" (String.replicate 165 "-")

    let printTableRow f f' f'' h (points : float list) i =
        let x_i = points.[i]
        let y_i = f x_i

        let f' = f' x_i
        let f'h2 = diffH2 f h points i
        let f'h4 = diffH4 f h points i

        let f'' = f'' x_i
        let f''h2 = diff2H2 f h points i

        match f'h2, f'h4, f''h2 with
        | Ok f'h2, Ok f'h4, Ok f''h2 ->
            let f'h2Err = abs (f' - f'h2)
            let f'h4Err = abs (f' - f'h4)
            let f''h2Err = abs (f'' - f''h2)
            printfn
                "%d\t%-12g\t%-12g\t%-12g\t%-12g\t%-12g\t%-12g\t%-12g\t%-12g\t%-12g\t%-12g"
                i
                x_i
                y_i
                f'
                f'h2
                f'h2Err
                f'h4
                f'h4Err
                f''
                f''h2
                f''h2Err
        | Error msg, _, _
        | _, Error msg, _
        | _, _, Error msg -> printfn "%d\t%-12g\t%-12g\t%s" i x_i y_i msg

    printTableHeader ()
    for i in seq { 0 .. List.length points - 1 } do
        printTableRow f f' f'' h points i
