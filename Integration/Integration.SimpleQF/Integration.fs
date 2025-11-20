module Integration

// ------------ Интегрирование с помощью простейших КФ ------------

let private integrateRectangleLeft f a b = (b - a) * f a

let private integrateRectangleRight f a b = (b - a) * f b

let private integrateRectangleMiddle f a b = (b - a) * f ((a + b) / 2.0)

let private integrateTrapezoid f a b = (b - a) / 2.0 * (f a + f b)

let private integrateSimpson f a b = (b - a) / 6.0 * (f a + 4.0 * f ((a + b) / 2.0) + f b)

// ------------ Работа с КФ ------------

type QuadratureForm =
    | RectangleLeft
    | RectangleRight
    | RectangleMiddle
    | Trapezoid
    | Simpson

let quadratureFormIntegrate =
    function
    | RectangleLeft -> integrateRectangleLeft
    | RectangleRight -> integrateRectangleRight
    | RectangleMiddle -> integrateRectangleMiddle
    | Trapezoid -> integrateTrapezoid
    | Simpson -> integrateSimpson

let quadratureFormStr =
    function
    | RectangleLeft -> "КФ левого прямоугольника"
    | RectangleRight -> "КФ правого прямоугольника"
    | RectangleMiddle -> "КФ среднего прямоугольника"
    | Trapezoid -> "КФ трапеции"
    | Simpson -> "КФ Симпсона"
