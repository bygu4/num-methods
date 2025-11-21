module Integration

open Functions
open QuadratureForms

// ------------ Вычисление через составные КФ ------------

let step a b m = (b - a) / float m

let integrateComposite quadForm f a b m =
    seq { 0 .. m - 1 }
    |> Seq.map (fun k ->
        quadratureFormIntegrate
            quadForm
            f
            (a + float k * step a b m)
            (a + float (k + 1) * step a b m))
    |> Seq.sum

// ------------ Уточнение по Рунге-Ромбергу ------------

let private refineByRunge quadForm J_m J_ml l =
    let r = float <| quadratureFormErrorOrder quadForm
    (float l ** r * J_ml - J_m) / (float l ** r - 1.0)

let printRefinedTable quadForms funcKind a b m l =
    let printTableHeader () =
        printfn "Название КФ\t\t\tJ\t\tJ(h)\t\t|J - J(h)|\tJ(h/l)\t\t|J - J(h/l)|\tJ_R\t\t|J - J_R|"
        printfn "%s" (String.replicate 140 "-")
    
    let printTableRow quadForm funcKind a b m l =
        let f = testFunc funcKind 0
        let J = testFuncInt funcKind 0 a b

        let J_m = integrateComposite quadForm f a b m
        let J_ml = integrateComposite quadForm f a b (m * l)
        let J_R = refineByRunge quadForm J_m J_ml l

        let J_m_err = abs (J - J_m)
        let J_ml_err = abs (J - J_ml)
        let J_R_err = abs (J - J_R)

        printfn
            "%-26s\t%-12g\t%-12g\t%-12g\t%-12g\t%-12g\t%-12g\t%-12g"
            (quadratureFormStr quadForm)
            J
            J_m
            J_m_err
            J_ml
            J_ml_err
            J_R
            J_R_err

    printTableHeader ()
    for quadForm in quadForms do
        printTableRow quadForm funcKind a b m l
