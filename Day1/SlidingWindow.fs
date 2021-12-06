module Day1.SlidingWindow

open System
open Xunit
open FsUnit.Xunit

let rec slidingWindowCollect (list: list<int>) : list<list<int>> =
    let len = List.length list

    match list with
    | _ when len = 3 -> list :: []
    | _ :: tail when List.length tail >= 2 -> List.take 3 list :: slidingWindowCollect tail
    | _ -> failwith "list needs >= 3 items"

let averageSlidingWindow list =
    list |> slidingWindowCollect |> List.map List.sum


[<Fact>]
let ``sliding window of 1 item is exception`` () =
    Assert.Throws<Exception>(fun () -> slidingWindowCollect [ 1 ] |> ignore)

[<Fact>]
let ``sliding window of 2 item is exception`` () =
    Assert.Throws<Exception>(fun () -> slidingWindowCollect [ 1; 2 ] |> ignore)

[<Fact>]
let ``sliding window of [1;2;3] item is [[1;2;3]]`` () =
    let data = slidingWindowCollect [ 1; 2; 3 ]
    data |> should equal [ [ 1; 2; 3 ] ]

[<Fact>]
let ``sliding window of [1,2,3,4] item is [[1;2;3];[2;3;4]`` () =
    // let data =
    let data = slidingWindowCollect [ 1; 2; 3; 4 ]
    data |> should equal [ [ 1; 2; 3 ]; [ 2; 3; 4 ] ]

[<Fact>]
let ``sliding window of [199;200;208;210;200;207;240;269;260;263] item is [[199;200;208];[200;208 ... etc`` () =
    let data =
        slidingWindowCollect [ 199
                               200
                               208
                               210
                               200
                               207
                               240
                               269
                               260
                               263 ]

    data
    |> should
        equal
        [ [ 199; 200; 208 ]
          [ 200; 208; 210 ]
          [ 208; 210; 200 ]
          [ 210; 200; 207 ]
          [ 200; 207; 240 ]
          [ 207; 240; 269 ]
          [ 240; 269; 260 ]
          [ 269; 260; 263 ] ]

[<Fact>]
let ``total sliding window of [199;200;208;210;200;207;240;269;260;263] item is 8 items`` () =
    let data =
        averageSlidingWindow [ 199
                               200
                               208
                               210
                               200
                               207
                               240
                               269
                               260
                               263 ]

    data
    |> should
        equal
        [ 607
          618
          618
          617
          647
          716
          769
          792 ]
