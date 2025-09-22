module Methods

let step A B N = (B - A) / double N
let left A B N i = A + step A B N * double i
let right A B N i = left A B N i + step A B N

let private log quiet fmt =
    Printf.kprintf (fun msg ->
        if not quiet then printfn "%s" msg
    ) fmt

// ------------ Отделение корней ------------

let separateRoots quiet (f : float -> float) A B N =
    log quiet "Начата процедура отделения корней"

    let h = step A B N
    let mutable cur = A
    let mutable i = 0

    [
    while cur < B do
        if f cur * f (cur + h) <= 0 then
            log quiet "Найден корень на %d-м отрезке: [%A, %A]" i cur (cur + h)
            yield i
        cur <- cur + h
        i <- i + 1
    ]

// ------------ Метод бисекции ------------

let getRootBisect quiet (f : float -> float) _ left right epsilon =
    log quiet "Начат поиск корня методом бисекции"

    let mutable left = left
    let mutable right = right
    let mutable mid = (left + right) / 2.0
    let mutable i = 0

    while right - left >= 2.0 * epsilon do
        log quiet "Шаг %d, текущий отрезок: [%A, %A]" i left right

        if f left * f mid <= 0 then
            right <- mid
        else
            left <- mid
        
        mid <- (left + right) / 2.0
        i <- i + 1

    log quiet "Шаг %d, текущий отрезок: [%A, %A]" i left right
    mid

// ------------ Метод Ньютона (+ модифицированный) ------------

let private getRootNewtonGeneric msg next quiet f f' left right epsilon =
    log quiet "%s" msg

    let x_0 = (left + right) / 2.0
    log quiet "0-е приближение: %A" x_0

    let mutable prev = x_0
    let mutable cur = next f f' prev x_0
    let mutable i = 1

    while abs (cur - prev) >= epsilon do
        log quiet "%d-е приближение: %A" i cur
        prev <- cur
        cur <- next f f' prev x_0
        i <- i + 1

    log quiet "%d-е приближение: %A" i cur
    cur

let getRootNewton =
    (fun f f' x_k _ -> x_k - f x_k / f' x_k)
    |> getRootNewtonGeneric "Начат поиск корня методом Ньютона"

let getRootNewtonMod =
    (fun f f' x_k x_0 -> x_k - f x_k / f' x_0)
    |> getRootNewtonGeneric "Начат поиск корня модифицированным методом Ньютона"

// ------------ Метод секущих ------------

let getRootSecant quiet f _ left right epsilon =
    log quiet "Начат поиск корня методом секущих"

    let mutable prev = left
    let mutable cur = right
    log quiet "0-е приближение: %A" prev
    log quiet "1-е приближение: %A" cur

    let next' f cur prev = cur - f cur * (cur - prev) / (f cur - f prev)
    let mutable next = next' f cur prev
    let mutable i = 2

    while abs (next - cur) >= epsilon do
        log quiet "%d-е приближение: %A" i next
        prev <- cur
        cur <- next
        next <- next' f cur prev
        i <- i + 1

    log quiet "%d-е приближение: %A" i next
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

let methodToStr =
    function
    | Bisect -> bisectMethodStr
    | Newton -> newtonMethodStr
    | NewtonMod -> newtonModMethodStr
    | Secant -> secantMethodStr

let methodToFunc =
    function
    | Bisect -> getRootBisect
    | Newton -> getRootNewton
    | NewtonMod -> getRootNewtonMod
    | Secant -> getRootSecant
