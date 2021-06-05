module TupleTests

open Tuple
open Xunit

[<Fact>]
let ``A tuple with w = 1.0 is a point``()=
    let px, py, pz, pw = 4.3, -4.2, 3.1, 1.0
    let tuple = rawTuple px py pz pw
    Assert.Equal(px, X tuple)
    Assert.Equal(py, Y tuple)
    Assert.Equal(pz, Z tuple)
    Assert.True(isPoint tuple)
    Assert.False(isVector tuple)

[<Fact>]
let ``A tuple with w = 0.0 is a vector``()=
    let px, py, pz, pw = 4.3, -4.2, 3.1, 0.0
    let tuple = rawTuple px py pz pw
    Assert.Equal(px, X tuple)
    Assert.Equal(py, Y tuple)
    Assert.Equal(pz, Z tuple)
    Assert.False(isPoint tuple)
    Assert.True(isVector tuple)

[<Fact>]
let ``point() creates tuples with w = 1.0``()=
    let px, py, pz = 4.0, -4.0, 3.0
    let testPoint = point px py pz
    Assert.Equal(px, X testPoint)
    Assert.Equal(py, Y testPoint)
    Assert.Equal(pz, Z testPoint)
    Assert.True(isPoint testPoint)
    Assert.False(isVector testPoint)

[<Fact>]
let ``vector() creates tuples with w = 0.0``()=
    let px, py, pz = 4.0, -4.0, 3.0
    let testVector = vector px py pz
    Assert.Equal(px, X testVector)    
    Assert.Equal(py, Y testVector)    
    Assert.Equal(pz, Z testVector)
    Assert.False(isPoint testVector)
    Assert.True(isVector testVector)

[<Fact>]
let ``Adding two tuples = tuple with properties added``()=
    let a1 = rawTuple 3.0 -2.0 5.0 1.0
    let a2 = rawTuple -2.0 3.0 1.0 0.0
    Assert.True(equal (rawTuple 1.0 1.0 6.0 1.0) (add a1 a2))
    Assert.True(equal (rawTuple 1.0 1.0 6.0 1.0) (a1 .+ a2))

[<Fact>]
let ``Subtracting two points = vector with properties subtracted``()=
    let p1 = point 3.0 2.0 1.0
    let p2 = point 5.0 6.0 7.0
    Assert.True(equal (vector -2.0 -4.0 -6.0) (subtract p1 p2))
    Assert.True(equal (vector -2.0 -4.0 -6.0) (p1 .- p2))

[<Fact>]
let ``Subtracting a vector from a point = point with properties subtracted``()=
    let p1 = point 3.0 2.0 1.0
    let v1 = vector 5.0 6.0 7.0
    Assert.True(equal (point -2.0 -4.0 -6.0) (subtract p1 v1))
    Assert.True(equal (point -2.0 -4.0 -6.0) (p1 .- v1))

[<Fact>]
let ``Subtracting two vectors = vector with properties subtracted``()=
    let v1 = vector 3.0 2.0 1.0
    let v2 = vector 5.0 6.0 7.0
    Assert.True(equal (vector -2.0 -4.0 -6.0) (subtract v1 v2))
    Assert.True(equal (vector -2.0 -4.0 -6.0) (v1 .- v2))

[<Fact>]
let ``Subtracting a vector from the zero vector = vector that points back``()=
    let zero = vector 0.0 0.0 0.0
    let v = vector 1.0 -2.0 3.0
    Assert.True(equal (vector -1.0 2.0 -3.0) (subtract zero v))
    Assert.True(equal (vector -1.0 2.0 -3.0) (zero .- v))

[<Fact>]
let ``Negating a tuple = tuple with properties reversed``()=
    let a = rawTuple 1.0 -2.0 3.0 -4.0
    Assert.True(equal (rawTuple -1.0 2.0 -3.0 4.0) (negate a))

[<Fact>]
let ``Multiplying by a scalar = properties multiplied by the scalar``()=
    let a = rawTuple 1.0 -2.0 3.0 -4.0
    Assert.True(equal (rawTuple 3.5 -7.0 10.5 -14.0) (multiply a 3.5))
    Assert.True(equal (rawTuple 3.5 -7.0 10.5 -14.0) (a .* 3.5))

[<Fact>]
let ``Multiplying by a fraction = properties multiplied by the fraction``()=
    let a = rawTuple 1.0 -2.0 3.0 -4.0
    Assert.True(equal (rawTuple 0.5 -1.0 1.5 -2.0) (multiply a 0.5))
    Assert.True(equal (rawTuple 0.5 -1.0 1.5 -2.0) (a .* 0.5))
    
[<Fact>]
let ``Dividing by a scalar = properties dvivided by the scalar``()=
    let a = rawTuple 1.0 -2.0 3.0 -4.0
    Assert.True(equal (rawTuple 0.5 -1.0 1.5 -2.0) (divide a 2.0))
    Assert.True(equal (rawTuple 0.5 -1.0 1.5 -2.0) (a ./ 2.0))

[<Fact>]
let ``Magnitude of vector(1, 0, 0) = 1``()=
    let v = vector 1.0 0.0 0.0
    Assert.Equal(1.0, magnitude v)

[<Fact>]
let ``Magnitude of vector(0, 1, 0) = 1``()=
    let v = vector 0.0 1.0 0.0
    Assert.Equal(1.0, magnitude v)

[<Fact>]
let ``Magnitude of vector(0, 0, 1) = 1``()=
    let v = vector 0.0 0.0 1.0
    Assert.Equal(1.0, magnitude v)

[<Fact>]
let ``Magnitude of vector(1, 2, 3) = sqrt 14``()=
    let v = vector 1.0 2.0 3.0
    Assert.Equal(sqrt 14.0, magnitude v)

[<Fact>]
let ``Normalizing vector(4, 0, 0) = vector(1, 0, 0)``()=
    let v = vector 4.0 0.0 0.0
    Assert.Equal(vector 1.0 0.0 0.0, normalize v)

[<Fact>]
let ``Normalizing vector(1, 2, 3) = vector(1/sqrt 14, 2/sqrt 14, 3/sqrt 14)``()=
    let v = vector 1.0 2.0 3.0
    Assert.Equal(vector (1.0 / sqrt 14.0) (2.0 / sqrt 14.0) (3.0 / sqrt 14.0), normalize v)
    Assert.Equal(1.0, magnitude(normalize v))

[<Fact>]
let ``Dot product of vector(1,2,3) & vector(2,3,4) = 20``()=
    let v1 = vector 1.0 2.0 3.0
    let v2 = vector 2.0 3.0 4.0
    Assert.Equal(20.0, dot v1 v2)

[<Fact>]
let ``Cross product of vector(1,2,3) & vector(2,3,4) = vector(-1,2,-1)``()=
    let v1 = vector 1.0 2.0 3.0
    let v2 = vector 2.0 3.0 4.0
    Assert.True(equal (vector -1.0 2.0 -1.0) (cross v1 v2))

[<Fact>]
let ``Cross product of vector(2,3,4) & vector(1,2,3) = vector(1,-2,1)``()=
    let v1 = vector 1.0 2.0 3.0
    let v2 = vector 2.0 3.0 4.0
    Assert.True(equal (vector 1.0 -2.0 1.0) (cross v2 v1))
