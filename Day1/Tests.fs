module Tests

open Xunit

let countIncreases list =
    match list with
    | [] -> 0
    | [ a ] -> 0
    | _ -> 0

[<Fact>]
let ``empty list`` () =
    // let data = [|199,200,208,210,200,207,240,269,260,263|]
    let count = countIncreases([])
    Assert.Equal(0, count)
    
[<Fact>]
let ``single item list`` () =
    let count = countIncreases([1])
    Assert.Equal(0, count)

[<Fact>]
let ``2 item list same`` () =
    let count = countIncreases([1,1])
    Assert.Equal(0, count)