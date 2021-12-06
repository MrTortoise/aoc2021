module Day1.SlidingWindow

open System
open System.IO

open Xunit
open FsUnit.Xunit

let rec slidingWindowCollect (list : list<int> ) : list<list<int>> =
    let len = List.length list
    match list with
    | _ when len = 3 -> list :: []
    | _ :: tail when List.length tail >= 2 -> List.take 3 list :: slidingWindowCollect tail
    | _ -> failwith "list needs >= 3 items"
    

[<Fact>]
let ``sliding window of 1 item is exception`` () = 
    Assert.Throws<Exception>(fun () -> slidingWindowCollect [1] |> ignore)

[<Fact>]
let ``sliding window of 2 item is exception`` () = 
      Assert.Throws<Exception>(fun () -> slidingWindowCollect [1;2] |> ignore)

[<Fact>]
let ``sliding window of 3 item is [[1;2;3]]`` () = 
    let data = slidingWindowCollect [1;2;3]
    data |> should equal [[1;2;3]]

[<Fact>]
let ``sliding window of [1,2,3,4] item is [[1;2;3];[2;34];[3;4],[4]`` () = 
    // let data = [|199;200;208;210;200;207;240;269;260;263|]
    let data = slidingWindowCollect [1;2;3;4]
    data |> should equal [[1;2;3];[2;3;4]]

[<Fact>]
let ``sliding window sum turns [1;2;3;4] into [6,9,7,4]`` () =
    List.length [1;2;3] > 2 |> should equal true 
   