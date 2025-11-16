module Functions

open System

// ------------ Тестовые функции ------------

let private f_1 x = x ** 2.0 + 3.0 * x - 5.0
let private f_1' x = 2.0 * x + 3.0
let private f_1'' _ = 2.0

let private f_2 x = 2.0 * x ** 3.0 - 5.0 * x ** 2.0 + 1.0
let private f_2' x = 6.0 * x ** 2.0 - 10.0 * x
let private f_2'' x = 12.0 * x - 10.0

let private f_3 x = Math.E ** x ** 2.0
let private f_3' x = 2.0 * x * Math.E ** x ** 2.0
let private f_3'' x = 2.0 * Math.E ** x ** 2.0 + 4.0 * x ** 2 * Math.E ** x ** 2

let private f_4 x = 4.0 * Math.Sin (x / 2.0) + x * Math.Cos (3.0 * x)
let private f_4' x = 2.0 * Math.Cos (x / 2.0) + Math.Cos (3.0 * x) - 3.0 * x * Math.Sin (3.0 * x)
let private f_4'' x = -Math.Sin (x / 2.0) - 3.0 * Math.Sin (3.0 * x) - 3.0 * Math.Sin (3.0 * x) - 9.0 * x * Math.Cos (3.0 * x)

// ------------ Работа с тестовыми функциями ------------

type FunctionKind =
    | SecondDegreePolynomial
    | ThirdDegreePolynomial
    | Exponent
    | Trigonometric

let testFunc  kind =
    match kind with
    | SecondDegreePolynomial -> f_1
    | ThirdDegreePolynomial -> f_2
    | Exponent -> f_3
    | Trigonometric -> f_4

let testFunc' kind =
    match kind with
    | SecondDegreePolynomial -> f_1'
    | ThirdDegreePolynomial -> f_2'
    | Exponent -> f_3'
    | Trigonometric -> f_4'

let testFunc'' kind =
    match kind with
    | SecondDegreePolynomial -> f_1''
    | ThirdDegreePolynomial -> f_2''
    | Exponent -> f_3''
    | Trigonometric -> f_4''

let testFuncStr kind =
    match kind with
    | SecondDegreePolynomial -> "x^2 + 3x - 5"
    | ThirdDegreePolynomial -> "2x^3 - 5x^2 + 1"
    | Exponent -> "e^(x^2)"
    | Trigonometric -> "4sin(x/2) + xcos(3x)"
