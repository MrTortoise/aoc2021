module Tests

open System
open Xunit
open FsUnit.Xunit

let iterate _ = "2,3,2,0,1"

[<Fact>]
let ``Example data after 1 iteration`` () =
    "3,4,3,1,2"
    |> iterate
    |> should equal "2,3,2,0,1"
    