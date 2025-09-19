open System

open Methods
open Console

let f r rho d = Math.PI / 3.0 * (d ** 3.0 - 3.0 * d ** 2.0 * r + 4.0 * r ** 3.0 * rho)
let f' r d = Math.PI / 3.0 * (3.0 * d ** 2.0 - 6.0 * d * r)
let f'' r d = Math.PI / 3.0 * (6.0 * d - 6.0 * r)

let table = [
    "Пробка", 0.25;
    "Бамбук", 0.4;
    "Сосна (белая)", 0.5;
    "Кедр", 0.55;
    "Дуб", 0.7;
    "Бук", 0.75;
    "Красное дерево", 0.8;
    "Тиковое дерево", 0.85;
    "Парафин", 0.9;
    "Лёд/Полиэтилен", 0.92;
    "Пчелиный воск", 0.95;
]

[<TailCall>]
let rec readRadius () =
    let r = readDouble "Введите радиус шара (в см): "

    if r > 0 then r / 100.0
    else

    printfn "Требуется положительный радиус, попробуйте снова"
    readRadius ()

let printTableWithDensity radius =
    let A = 0
    let B = 2.0 * radius
    let N = 1000
    let epsilon = 1e-09
    let method = Newton

    let printTableHeader () =
        printfn "\n\tВещество\tПлотность (г/мл)\tГлубина погружения (см)"
        printfn "%s" (String.replicate 71 "-")

    let printTableRow i material density depth =
        printfn "%d\t%-16s%A\t\t\t%A" i material density (depth * 100.0)

    printTableHeader ()
    let mutable i = 1
    for material, density in table do
        let depth =
            try
                let sector = List.head <| separateRoots true (f radius density) A B N
                let left = left A B N sector
                let right = right A B N sector
                methodToFunc method true (f radius density) (f' radius) left right epsilon
            with :? ArgumentException -> nan

        printTableRow i material density depth
        i <- i + 1

readRadius () |> printTableWithDensity
