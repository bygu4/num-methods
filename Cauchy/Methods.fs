module Methods

open System

open Common

let private computeGeneric quiet f x_0 y_0 h N next =
    let rec helper x_k y_k curN =
        log quiet "y_%d = %A" (N - curN) y_k

        match curN with
        | 0 -> y_k
        | n when n > 0 -> helper (x_k + h) (next f x_k y_k h) (curN - 1)
        | _ -> raise <| ArgumentException $"Expected non-negative N, but got: {curN}"

    in helper x_0 y_0 N

// ------------ Методы Эйлера ------------

let computeEuler quiet f x_0 y_0 h N =
    (fun f x_k y_k h -> y_k + h * f x_k y_k)
    |> computeGeneric quiet f x_0 y_0 h N

let computeEulerI quiet f x_0 y_0 h N =
    (fun f x_k y_k h -> y_k + h * f (x_k + h / 2.0) (y_k + h / 2.0 * f x_k y_k))
    |> computeGeneric quiet f x_0 y_0 h N

let computeEulerII quiet f x_0 y_0 h N =
    (fun f x_k y_k h -> y_k + h / 2.0 * (f x_k y_k + f (x_k + h) (y_k + h * f x_k y_k)))
    |> computeGeneric quiet f x_0 y_0 h N

// ------------ Метод Рунге-Кутты ------------

let computeRungeKutta quiet f x_0 y_0 h N =
    (fun f x_k y_k h ->
        let k1 = h * f x_k y_k
        let k2 = h * f (x_k + h / 2.0) (y_k + k1 / 2.0)
        let k3 = h * f (x_k + h / 2.0) (y_k + k2 / 2.0)
        let k4 = h * f (x_k + h) (y_k + k3)
        y_k + 1.0 / 6.0 * (k1 + 2.0 * k2 + 2.0 * k3 + k4)
    ) |> computeGeneric quiet f x_0 y_0 h N

// ------------ Работа с численными методами ------------

type NumMethod =
    | Euler
    | EulerI
    | EulerII
    | RungeKutta

let compute =
    function
    | Euler -> computeEuler
    | EulerI -> computeEulerI
    | EulerII -> computeEulerII
    | RungeKutta -> computeRungeKutta

let numMethodStr =
    function
    | Euler -> "метод Эйлера"
    | EulerI -> "метод Эйлера I"
    | EulerII -> "метод Эйлера II"
    | RungeKutta -> "метод Рунге-Кутты"

let numMethodErrorOrder =
    function
    | Euler -> 1
    | EulerI -> 2
    | EulerII -> 2
    | RungeKutta -> 4

// ------------ Уточнение по Рунге-Ромбергу ------------

let refineByRunge method J_h J_hl l =
    let r = float <| numMethodErrorOrder method
    (float l ** r * J_hl - J_h) / (float l ** r - 1.0)
