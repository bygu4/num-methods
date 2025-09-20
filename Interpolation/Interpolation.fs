module Interpolation

open System

let private log quiet fmt =
    Printf.kprintf (fun msg ->
        if not quiet then printfn "%s" msg
    ) fmt

// ------------ Узлы интерполирования ------------

type PointType =
    | Equidistant
    | Random

[<Literal>]
let equidistantPointStr = "равноудалённые"

[<Literal>]
let randomPointStr = "случайные"

let pointTypeToStr =
    function
    | Equidistant -> equidistantPointStr
    | Random -> randomPointStr

let generateEquidistantPoints A B m =
    let mutable cur = A

    [
    for _ in { 1 .. m } do
        yield cur
        cur <- cur + (B - A) / double (m - 1)
    ]

let generateRandomPoints A B m =
    let mutable points = []

    let rec nextRandomPoint A B points =
        let next = Random.Shared.NextDouble () * (B - A) + A

        if not <| List.contains next points then
            next
        else
            nextRandomPoint A B points

    for _ in { 1 .. m } do
        points <- nextRandomPoint A B points :: points

    points

let generatePoints =
    function
    | Equidistant -> generateEquidistantPoints
    | Random -> generateRandomPoints

let sortFromPoint (x : double) =
    List.sortBy (fun x_i -> abs (x - x_i))

let printPointsTable f (points : double seq) =
    let printTableHeader () =
        printfn "i\tx_i\t\tf(x_i)"
        printfn "%s" (String.replicate 36 "-")

    let printTableRow i x_i f =
        printfn "%d\t%-12g\t%-12g" i x_i (f x_i)

    printTableHeader ()
    let mutable i = 0
    for x_i in points do
        printTableRow i x_i f
        i <- i + 1

let printSortedTable f (points : double seq) x =
    let printTableHeader () =
        printfn "i\tx_i\t\t|x - x_i|\tf(x_i)"
        printfn "%s" (String.replicate 52 "-")

    let printTableRow i x_i x f =
        printfn "%d\t%-12g\t%-12g\t%-12g" i x_i (abs (x - x_i)) (f x_i)

    printTableHeader ()
    let mutable i = 0
    for x_i in points do
        printTableRow i x_i x f
        i <- i + 1

// ------------ Представление в форме Лагранжа ------------

let private t points i x =
    points
    |> List.map (fun x_i -> x - x_i)
    |> List.removeAt i
    |> List.reduce ( * )

let private l points i x =
    t points i x / t points i points.[i]

let interpolateLagrange quiet f (points : double list) x =
    let coefficients = List.mapi (fun i _ -> l points i x) points
    List.iteri (log quiet "Значение %d-го лагранжевого коэффициента: %A") coefficients
    log quiet "Сумма значений лагранжевых коэффициентов: %A" (List.sum coefficients)

    coefficients
    |> List.mapi (fun i c -> c * f points.[i])
    |> List.sum

// ------------ Представление в форме Ньютона ------------

let private getCoefficients f points =
    let len = List.length points

    let mutable table = [ Array.ofList <| List.map f points ]
    for i in { 1 .. len - 1 } do
        table <- [|
        for j in { 1 .. len - i } do
            yield (table.Head.[j] - table.Head.[j - 1]) / (points.[j + i - 1] - points.[j - 1])
        |] :: table

    table |> List.rev |> List.map Array.head

let interpolateNewton quiet f (points : double list) x =
    let len = List.length points
    let coefficients = getCoefficients f points
    List.iteri (log quiet "Коэффициент A_%d: %A") coefficients

    let mutable res = coefficients.[len - 1]
    for i in { len - 2 .. -1 .. 0 } do
        res <- coefficients.[i] + (x - points.[i]) * res

    res

// ------------ Работа с методами интерполирования ------------

type InterpolationMethod =
    | Lagrange
    | Newton

[<Literal>]
let lagrangeMethodStr = "представление в форме Лагранжа"

[<Literal>]
let newtonMethodStr = "представление в форме Ньютона"

let methodToStr =
    function
    | Lagrange -> lagrangeMethodStr
    | Newton -> newtonMethodStr

let methodToFunc =
    function
    | Lagrange -> interpolateLagrange
    | Newton -> interpolateNewton
