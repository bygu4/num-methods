open System

let f x = sqrt (4.0 * x + 7.0) - 3.0 * cos x
let f' x = 2.0 / sqrt (4.0 * x + 7.0) + 3.0 * sin x
let f'' x = -4.0 / (4.0 * x + 7.0) ** (3.0 / 2.0) + 3.0 * cos x

let step A B N = (B - A) / double N

// ------------ Пользовательский ввод ------------

let rec readInt msg =
    printf msg
    let input = Console.ReadLine ()

    match Int32.TryParse input with
    | true, result -> result
    | false, _ ->
        printf "Не удалось распознать число, попробуйте снова"
        readInt msg

let rec readDouble msg =
    printf msg
    let input = Console.ReadLine ()

    match Double.TryParse input with
    | true, result -> result
    | false, _ ->
        printf "Не удалось распознать число, попробуйте снова"
        readDouble msg

let rec readSectionBounds () =
    let A = readDouble "Введите левую границу отрезка (A): "
    let B = readDouble "Введите правую границу отрезка (B): "

    if A < B then A, B
    else

    printf "Требуется A < B, попробуйте снова"
    readSectionBounds ()

let rec readNumberOfSections () =
    let N = readDouble "Введите число отрезков табуляции (N): "

    if N >= 2 then N
    else

    printf "Требуется N >= 2, попробуйте снова"
    readNumberOfSections ()

let rec readEpsilon () =
    let epsilon = readDouble "Введите точность вычисления (epsilon): "

    if epsilon > 0 then epsilon
    else

    printf "Требуется epsilon > 0, попробуйте снова"
    readEpsilon ()

let rec readTargetSection (sections : int list) =
    let i = readInt "Введите номер целевого отрезка (i): "

    if List.contains i sections then i
    else

    printf "На данном отрезке не было найдено корней, попробуйте снова"
    readTargetSection sections

// ------------ Отделение корней ------------

let separateRoots A B N =
    printf "Начата процедура отделения корней"

    let h = step A B N
    let mutable cur = A
    let mutable i = 0

    [
    while cur < B do
        if f cur * f (cur + h) <= 0 then
            printf "Найден корень на %d-м отрезке: [%f, %f]" i cur (cur + h)
            yield i
        cur <- cur + h
        i <- i + 1
    ]

// ------------ Метод бисекции ------------

let getRootBisect left right epsilon =
    printf "Начат поиск корня методом бисекции"

    let mutable left = left
    let mutable right = right
    let mutable mid = (left + right) / 2.0

    while right - left >= 2.0 * epsilon do
        printf "Текущий отрезок: [%f, %f]" left right

        if f left * f mid <= 0 then
            right <- mid
        else
            left <- mid
        
        mid <- (left + right) / 2.0

    mid

// ------------ Метод Ньютона (+ модифицированный) ------------

let getRootNewtonGeneric msg next left right epsilon =
    printf msg

    let x_0 = (left + right) / 2.0
    printf "Первое приближение: %f" x_0

    let mutable prev = x_0
    let mutable cur = next prev x_0

    while abs (cur - prev) >= epsilon do
        printf "Следующее приближение: %f" cur
        prev <- cur
        cur <- next prev x_0

    cur

let getRootNewton =
    (fun x_k _ -> x_k - f x_k / f' x_k)
    |> getRootNewtonGeneric "Начат поиск корня методом Ньютона"

let getRootNewtonMod =
    (fun x_k x_0 -> x_k - f x_k / f' x_0)
    |> getRootNewtonGeneric "Начат поиск корня модифицированным методом Ньютона"

// ------------ Метод секущих ------------

let getRootSecant left right epsilon =
    printf "Начат поиск корня методом секущих"

    let mutable prev = left
    let mutable cur = right
    printf "Первое приближение: %f" prev
    printf "Второе приближение: %f" cur

    let next' cur prev = cur - f cur / (f cur - f prev)
    let mutable next = next' cur prev

    while abs (next - cur) >= epsilon do
        printf "Следующее приближение: %f" next
        prev <- cur
        cur <- next
        next <- next' cur prev

    next
