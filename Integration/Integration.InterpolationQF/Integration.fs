module Integration

open MathNet.Numerics.LinearAlgebra

open Common
open Functions

// ------------ Интегрирование с помощью интерполяционных КФ ------------

let private getCoefficients quiet a b points =
    let N = List.length points
    
    let A =
        [ 0 .. N - 1 ]
        |> List.map (fun i ->
            List.map (fun x -> x ** float i) points)
        |> matrix

    let b =
        [ 0 .. N - 1]
        |> List.map (momentum a b)
        |> vector

    log quiet "Вычисленные моменты:"
    b.ToArray() |> Array.iteri (log quiet "\tµ_%d = %.12g")

    A.Solve(b).ToArray()

let private printCoefficientsTable points (coefficients : float array) =
    let printTableHeader () =
        printfn "i\tx_i\t\tA_i"
        printfn "%s" (String.replicate 38 "-")
    
    let printTableRow i x_i A_i = printfn "%d\t%-12g\t%.12g" i x_i A_i

    printTableHeader ()
    for i in seq { 0 .. List.length points - 1 } do
        printTableRow (i + 1) points.[i] coefficients.[i]

let integrateIQF quiet f a b points =
    let coefficients = getCoefficients quiet a b points

    if not quiet then
        printf "\n"
        printCoefficientsTable points coefficients

    List.map2 (fun A x -> A * f x) (List.ofArray coefficients) points
    |> List.sum

let checkErrorIQF quiet a b points =
    let N = List.length points
    let f = testFunc NthDegreePolynomial (N - 1)
    let coefficients = getCoefficients true a b points

    log quiet $"\nПроверка на точность ИКФ для g(x) = {testFuncStr NthDegreePolynomial}"
    let expected = testFuncWeighedInt NthDegreePolynomial (N - 1) a b

    let actual =
        List.map2 (fun A x -> A * f x) (List.ofArray coefficients) points
        |> List.sum

    let error = abs (expected - actual)
    log quiet "Абсолютная погрешность для интеграла ρ(x)g(x): %.12g" error
    error
