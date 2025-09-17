module Methods

// ------------ Отделение корней ------------

let step A B N = (B - A) / double N
let left A B N i = A + step A B N * double i
let right A B N i = left A B N i + step A B N

let separateRoots (f : float -> float) A B N =
    printfn "Начата процедура отделения корней"

    let h = step A B N
    let mutable cur = A
    let mutable i = 0

    [
    while cur < B do
        if f cur * f (cur + h) <= 0 then
            printfn "Найден корень на %d-м отрезке: [%A, %A]" i cur (cur + h)
            yield i
        cur <- cur + h
        i <- i + 1
    ]

// ------------ Метод бисекции ------------

let getRootBisect (f : float -> float) _ left right epsilon =
    printfn "Начат поиск корня методом бисекции"

    let mutable left = left
    let mutable right = right
    let mutable mid = (left + right) / 2.0
    let mutable i = 0

    while right - left >= 2.0 * epsilon do
        printfn "Шаг %d, текущий отрезок: [%A, %A]" i left right

        if f left * f mid <= 0 then
            right <- mid
        else
            left <- mid
        
        mid <- (left + right) / 2.0
        i <- i + 1

    mid

// ------------ Метод Ньютона (+ модифицированный) ------------

let private getRootNewtonGeneric msg next f f' left right epsilon =
    printfn "%s" msg

    let x_0 = (left + right) / 2.0
    printfn "0-е приближение: %A" x_0

    let mutable prev = x_0
    let mutable cur = next f f' prev x_0
    let mutable i = 1

    while abs (cur - prev) >= epsilon do
        printfn "%d-е приближение: %A" i cur
        prev <- cur
        cur <- next f f' prev x_0
        i <- i + 1

    cur

let getRootNewton =
    (fun f f' x_k _ -> x_k - f x_k / f' x_k)
    |> getRootNewtonGeneric "Начат поиск корня методом Ньютона"

let getRootNewtonMod =
    (fun f f' x_k x_0 -> x_k - f x_k / f' x_0)
    |> getRootNewtonGeneric "Начат поиск корня модифицированным методом Ньютона"

// ------------ Метод секущих ------------

let getRootSecant f _ left right epsilon =
    printfn "Начат поиск корня методом секущих"

    let mutable prev = left
    let mutable cur = right
    printfn "0-е приближение: %A" prev
    printfn "1-е приближение: %A" cur

    let next' f cur prev = cur - f cur * (cur - prev) / (f cur - f prev)
    let mutable next = next' f cur prev
    let mutable i = 2

    while abs (next - cur) >= epsilon do
        printfn "%d-е приближение: %A" i next
        prev <- cur
        cur <- next
        next <- next' f cur prev
        i <- i + 1

    next

// ------------ Работа с методами вычислений ------------

type ComputationMethod =
    | Bisect
    | Newton
    | NewtonMod
    | Secant

[<Literal>]
let bisectMethodStr = "метод бисекции"

[<Literal>]
let newtonMethodStr = "метод Ньютона"

[<Literal>]
let newtonModMethodStr = "модифицированный метод Ньютона"

[<Literal>]
let secantMethodStr = "метод секущих"

let methodToStr method =
    match method with
    | Bisect -> bisectMethodStr
    | Newton -> newtonMethodStr
    | NewtonMod -> newtonModMethodStr
    | Secant -> secantMethodStr

let methodToFunc method =
    match method with
    | Bisect -> getRootBisect
    | Newton -> getRootNewton
    | NewtonMod -> getRootNewtonMod
    | Secant -> getRootSecant
