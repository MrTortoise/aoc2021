module Day2.CalculatePositionScalar

open System
open Xunit
open FsUnit.Xunit

let calculatePositionScalar (input : string) =
    let items = input.Split(" ")
    int(items.[1])
    

[<Fact>]
let ``Forward 5 should result in 5`` () =
    calculatePositionScalar "Forward 5" |> should equal 5
    