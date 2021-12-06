module Day1.CountIncreases

open System.IO
open Xunit

let rec countIncreases list =
    let increasing (l: int, r: int) = if r > l then 1 else 0

    match list with
    | [] -> 0
    | [ _ ] -> 0
    | [ a; b ] -> increasing (a, b)
    | l :: r :: tail -> increasing (l, r) + countIncreases (r :: tail)

[<Fact>]
let ``empty list`` () =
    // let data = [|199;200;208;210;200;207;240;269;260;263|]
    let count = countIncreases ([])
    Assert.Equal(0, count)

[<Fact>]
let ``single item list`` () =
    let count = countIncreases ([ 1 ])
    Assert.Equal(0, count)

[<Fact>]
let ``2 item list same`` () =
    let count = countIncreases ([ 1; 1 ])
    Assert.Equal(0, count)

[<Fact>]
let ``2 item list decreasing`` () =
    let count = countIncreases ([ 2; 1 ])
    Assert.Equal(0, count)

[<Fact>]
let ``2 item list increasing`` () =
    let count = countIncreases ([ 1; 2 ])
    Assert.Equal(1, count)

[<Fact>]
let ``3 item list not increasing`` () =
    let count = countIncreases ([ 2; 2; 1 ])
    Assert.Equal(0, count)

[<Fact>]
let ``3 item list increasing`` () =
    let count = countIncreases ([ 1; 2; 3 ])
    Assert.Equal(2, count)

[<Fact>]
let ``example  list`` () =
    let data =
        [ 199
          200
          208
          210
          200
          207
          240
          269
          260
          263 ]

    let count = countIncreases data
    Assert.Equal(7, count)


[<Fact>]
let ``the days data adds up to the right amount`` () =
    let data =
        File.ReadLines("Day1Data.txt")
        |> Seq.map (int)
        |> Seq.toList

    let count = countIncreases data
    Assert.Equal(1527, count)
