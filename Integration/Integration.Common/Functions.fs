module Functions

open System

open MathNet.Numerics.Integration

let defInt a b antiDerivative = antiDerivative b 0.0 - antiDerivative a 0.0

// ------------ Весовые функции ------------

let weightFunc x = x ** (-1.0 / 4.0)

let weighed f x =  weightFunc x * f x

let momentum a b k =
    fun x c -> 1.0 / (float k + 3.0 / 4.0) * x ** (float k + 3.0 / 4.0) + c
    |> defInt a b

// ------------ Тестовые функции ------------

let private f_0 _ = 42.0
let private F_0 x c  = 42.0 * x + c
let private F_0_weighed x c = 56.0 * x ** (3.0 / 4.0) + c

let private f_1 x = 4.0 * x - 7.0
let private F_1 x c = 2.0 * x ** 2.0 - 7.0 * x + c
let private F_1_weighed x c = 4.0 / 21.0 * x ** (3.0 / 4.0) * (12.0 * x - 49.0) + c

let private f_2 x = x ** 2.0 - 1.0
let private F_2 x c = 1.0 / 3.0 * x ** 3.0 - x + c
let private F_2_weighed x c = 4.0 / 33.0 * x ** (3.0 / 4.0) * (3.0 * x ** 2.0 - 11.0) + c

let private f_3 x = -2.0 * x ** 3.0 + 5.0 * x - 10.0
let private F_3 x c = -1.0 / 2.0 * x ** 4.0 + 5.0 / 2.0 * x ** 2.0 - 10.0 * x + c
let private F_3_weighed x c = -4.0 / 105.0 * x ** (3.0 / 4.0) * (14.0 * x ** 3.0 - 75.0 * x + 350.0) + c

let private f_4 n x =
    Seq.initInfinite (fun i -> -1.0 ** float i * x ** float i)
    |> Seq.take (n + 1)
    |> Seq.sum

let private F_4 n x c =
    Seq.initInfinite (fun i -> -1.0 ** float i / float (i + 1) * x ** float (i + 1))
    |> Seq.take (n + 1)
    |> Seq.sum
    |> ( + ) c

let private F_4_weighed n x c =
    Seq.initInfinite (fun i -> -1.0 ** float i / (float i + 3.0 / 4.0) * x ** (float i + 3.0 / 4.0))
    |> Seq.take (n + 1)
    |> Seq.sum
    |> ( + ) c

let private f_5 = Math.Sin
let private F_5 x c = -Math.Cos x + c

// ------------ Работа с тестовыми функциями ------------

type FunctionKind =
    | Constant
    | FirstDegreePolynomial
    | SecondDegreePolynomial
    | ThirdDegreePolynomial
    | NthDegreePolynomial
    | Trigonometric

let testFunc n =
    function
    | Constant -> f_0
    | FirstDegreePolynomial -> f_1
    | SecondDegreePolynomial -> f_2
    | ThirdDegreePolynomial -> f_3
    | NthDegreePolynomial -> f_4 n
    | Trigonometric -> f_5

let testFuncInt n a b =
    function
    | Constant -> defInt a b F_0
    | FirstDegreePolynomial -> defInt a b F_1
    | SecondDegreePolynomial -> defInt a b F_2
    | ThirdDegreePolynomial -> defInt a b F_3
    | NthDegreePolynomial -> defInt a b (F_4 n)
    | Trigonometric -> defInt a b F_5

let testFuncWeighedInt n a b =
    function
    | Constant -> defInt a b F_0_weighed
    | FirstDegreePolynomial -> defInt a b F_1_weighed
    | SecondDegreePolynomial -> defInt a b F_2_weighed
    | ThirdDegreePolynomial -> defInt a b F_3_weighed
    | NthDegreePolynomial -> defInt a b (F_4_weighed n)
    | Trigonometric -> DoubleExponentialTransformation.Integrate(weighed f_5, a, b, 1e-09)

let testFuncStr =
    function
    | Constant -> "42"
    | FirstDegreePolynomial -> "4x - 7"
    | SecondDegreePolynomial -> "x^2 - 1"
    | ThirdDegreePolynomial -> "-2x^3 + 5x - 10"
    | NthDegreePolynomial -> "\\sum_{i=0}^{N-1} (-1)^i * x^i"
    | Trigonometric -> "sin(x)"
