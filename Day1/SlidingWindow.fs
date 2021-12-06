﻿module Day1.SlidingWindow

open System.IO
open Xunit
open FsUnit.Xunit

let rec slidingWindow list =
    match list with
    | [h] -> [[h]]
    | i :: [j] -> [[i;j];[j]]
    | i :: j :: [k] -> [[i;j;k];[j;k];[k]]
    | _ -> []

[<Fact>]
let ``sliding window of 1 item is [[1]]`` () = 
    // let data = [|199;200;208;210;200;207;240;269;260;263|]
    let data = slidingWindow [1]
    data |> should equal [[1]]

[<Fact>]
let ``sliding window of 2 item is [[1;2];[2]]`` () = 
    // let data = [|199;200;208;210;200;207;240;269;260;263|]
    let data = slidingWindow [1;2]
    data |> should equal [[1;2];[2]]

[<Fact>]
let ``sliding window of 3 item is [[1;2;3];[2;3];[3]]`` () = 
    // let data = [|199;200;208;210;200;207;240;269;260;263|]
    let data = slidingWindow [1;2;3]
    data |> should equal [[1;2;3];[2;3];[3]]