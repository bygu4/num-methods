open System

let f x = sqrt (4.0 * x + 7.0) - 3.0 * cos x
let f' x = 2.0 / sqrt (4.0 * x + 7.0) + 3.0 * sin x
let f'' x = -4.0 / (4.0 * x + 7.0) ** (3.0 / 2.0) + 3.0 * cos x

let step A B N = (B - A) / double N
let left A B N i = A + step A B N * double i
let right A B N i = left A B N i + step A B N

// ------------ Отделение корней ------------

let separateRoots A B N =
    printfn "Начата процедура отделения корней"

    let h = step A B N
    let mutable cur = A
    let mutable i = 0

    [
    while cur < B do
        if f cur * f (cur + h) <= 0 then
            printfn "Найден корень на %d-м отрезке: [%g, %g]" i cur (cur + h)
            yield i
        cur <- cur + h
        i <- i + 1
    ]

// ------------ Метод бисекции ------------

let getRootBisect left right epsilon =
    printfn "Начат поиск корня методом бисекции"

    let mutable left = left
    let mutable right = right
    let mutable mid = (left + right) / 2.0
    let mutable i = 0

    while right - left >= 2.0 * epsilon do
        printfn "Шаг %d, текущий отрезок: [%g, %g]" i left right

        if f left * f mid <= 0 then
            right <- mid
        else
            left <- mid
        
        mid <- (left + right) / 2.0
        i <- i + 1

    mid

// ------------ Метод Ньютона (+ модифицированный) ------------

let getRootNewtonGeneric msg next left right epsilon =
    printfn "%s" msg

    let x_0 = (left + right) / 2.0
    printfn "0-е приближение: %g" x_0

    let mutable prev = x_0
    let mutable cur = next prev x_0
    let mutable i = 1

    while abs (cur - prev) >= epsilon do
        printfn "%d-е приближение: %g" i cur
        prev <- cur
        cur <- next prev x_0
        i <- i + 1

    cur

let getRootNewton =
    (fun x_k _ -> x_k - f x_k / f' x_k)
    |> getRootNewtonGeneric "Начат поиск корня методом Ньютона"

let getRootNewtonMod =
    (fun x_k x_0 -> x_k - f x_k / f' x_0)
    |> getRootNewtonGeneric "Начат поиск корня модифицированным методом Ньютона"

// ------------ Метод секущих ------------

let getRootSecant left right epsilon =
    printfn "Начат поиск корня методом секущих"

    let mutable prev = left
    let mutable cur = right
    printfn "0-е приближение: %g" prev
    printfn "1-е приближение: %g" cur

    let next' cur prev = cur - f cur * (cur - prev) / (f cur - f prev)
    let mutable next = next' cur prev
    let mutable i = 2

    while abs (next - cur) >= epsilon do
        printfn "%d-е приближение: %g" i next
        prev <- cur
        cur <- next
        next <- next' cur prev
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

// ------------ Меню ------------

type FirstMenuAction =
    | ExitProgram
    | ReadSectionBounds
    | ReadNumberOfSections
    | ReadEpsilon
    | StartRootSeparation

type SecondMenuAction =
    | ExitMenu
    | ReadTargetSection
    | ReadMethod
    | StartComputation

[<TailCall>]
let rec readFirstMenuAction () =
    printfn "
Доступные действия:
\t0 --- выход из программы
\t1 --- выбор границ отрезка
\t2 --- выбор числа отрезков табуляции
\t3 --- выбор точности вычисления
\t4 --- начало отделения корней
"
    match readInt "Выберите действие: " with
    | 0 -> ExitProgram
    | 1 -> ReadSectionBounds
    | 2 -> ReadNumberOfSections
    | 3 -> ReadEpsilon
    | 4 -> StartRootSeparation
    | _ ->
        printfn "Неизвестное действие, попробуйте снова"
        readFirstMenuAction ()

[<TailCall>]
let rec readSecondMenuAction () =
    printfn "
Доступные действия:
\t0 --- возврат к предыдущему меню
\t1 --- выбор целевого отрезка
\t2 --- выбор метода уточнения корня
\t3 --- начало уточнения корня
"
    match readInt "Выберите действие: " with
    | 0 -> ExitMenu
    | 1 -> ReadTargetSection
    | 2 -> ReadMethod
    | 3 -> StartComputation
    | _ -> 
        printfn "Неизвестное действие, попробуйте снова"
        readSecondMenuAction ()

let printFirstMenuParams A B N epsilon =
    printfn "
[A, B] = [%g, %g]
N = %d
h = %g
epsilon = %g" A B N (step A B N) epsilon

let printSecondMenuParams A B N i method =
    printfn "
Уточнение на отрезке [%g, %g]
Используется %s" (left A B N i) (right A B N i) (methodToStr method)

[<TailCall>]
let rec openFirstMenu A B N epsilon =
    printFirstMenuParams A B N epsilon

    let rec openSecondMenu A B N epsilon sections i method =
        printSecondMenuParams A B N i method

        match readSecondMenuAction () with
        | ExitMenu -> openFirstMenu A B N epsilon
        | ReadTargetSection ->
            let i = readTargetSection sections
            openSecondMenu A B N epsilon sections i method
        | ReadMethod ->
            let method = readMethod ()
            openSecondMenu A B N epsilon sections i method
        | StartComputation ->
            let left = left A B N i
            let right = right A B N i
            let root = methodToFunc method left right epsilon
            printfn "Найден корень требуемой точности: %g" root
            printfn "Вычисленная невязка: %g" (f root)
            openSecondMenu A B N epsilon sections i method

    match readFirstMenuAction () with
    | ExitProgram -> exit 0
    | ReadSectionBounds ->
        let A, B = readSectionBounds ()
        openFirstMenu A B N epsilon
    | ReadNumberOfSections ->
        let N = readNumberOfSections ()
        openFirstMenu A B N epsilon
    | ReadEpsilon ->
        let epsilon = readEpsilon ()
        openFirstMenu A B N epsilon
    | StartRootSeparation ->
        match separateRoots A B N with
        | [] ->
            printfn "Корней не найдено, попробуйте другие параметры"
            openFirstMenu A B N epsilon
        | _ as sections ->
            let i = readTargetSection sections
            let method = readMethod ()
            openSecondMenu A B N epsilon sections i method

// ------------ Точка входа ------------

let A, B = readSectionBounds ()
let N = readNumberOfSections ()
let epsilon = readEpsilon ()
openFirstMenu A B N epsilon
