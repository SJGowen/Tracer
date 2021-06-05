module Tuple

let private epsilon = 0.00001
let private wPoint = 1.0
let private wVector = 0.0

type Tuple = Tuple of (struct (float * float * float * float))
let rawTuple  x y z w : Tuple = Tuple (x, y, z, w)
let X (Tuple (x, _, _, _))  = x
let Y (Tuple (_, y, _, _))  = y
let Z (Tuple (_, _, z, _))  = z
let W (Tuple (_, _, _, w))  = w

//type Point = Tuple of (struct (float * float * float))
let point x y z = rawTuple x y z wPoint
let isPoint t = W t = wPoint

//type Vector = Tuple of (struct (float * float * float))
let vector x y z = rawTuple x y z wVector
let isVector t = W t = wVector

let private valEqual a b = a - b |> abs |> (>) epsilon
let equal a b =
    valEqual (X a) (X b)
    && valEqual (Y a) (Y b)
    && valEqual (Z a) (Z b)
    && valEqual (W a) (W b)

let add a b = rawTuple (X a + X b) (Y a + Y b) (Z a + Z b) (W a + W b)
let (.+) = add

let subtract a b = rawTuple (X a - X b) (Y a - Y b) (Z a - Z b) (W a - W b)
let (.-) = subtract

let negate a = rawTuple -(X a) -(Y a) -(Z a) -(W a)

let multiply a multiplier = rawTuple (X a * multiplier) (Y a * multiplier) (Z a * multiplier) (W a * multiplier)
let (.*) = multiply

let divide a divisor = rawTuple (X a / divisor) (Y a / divisor) (Z a / divisor) (W a / divisor)
let (./) = divide

let magnitude a = sqrt((X a) * (X a) + (Y a) * (Y a) + (Z a) * (Z a))

let normalize a = vector (X a / magnitude a) (Y a / magnitude a) (Z a / magnitude a)

let dot a b = X a * X b + Y a * Y b + Z a * Z b + W a * W b

let cross a b = vector (Y a * Z b - Z a * Y b) (Z a * X b - X a * Z b) (X a * Y b - Y a * X b)