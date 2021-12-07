module Day3.Scrubbers

open System
open Xunit
open FsUnit.Xunit

let scrubUp carbon oxygen = carbon * oxygen

let stringToStringList (input: string) =
    input.Trim().Split("\n")
    |> Array.map (fun i-> i.Trim())
    |> Seq.toList

//let co2 (input: string) =
//    input.Trim().Split("\n")
//    |> Array.mapi fun i d -> 
    
let findOxygenRating (index: int) (rows: list<string>) =
    let midPoint = rows.Length/2
    
    let total =
        rows
        |> List.map (fun i -> i.[index])
        |> List.filter (fun i -> i = '1')
        |> List.length
    
    if total > midPoint then "1" else "0"

let calculateScrubberScalar _ = 0
    

[<Fact>]
let ``Calculate the scrubber scalar`` () =
    scrubUp 2 2 |> should equal 4

[<Fact>]
let ``oxygen rating at index 0 is 1`` () =
    let input = """00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"""

    input
    |> stringToStringList
    |> findOxygenRating(0)
    |> should equal "1"


[<Fact>]
let ``single row with a 0 is 0`` () =
    let input = """00100"""

    input
    |> calculateScrubberScalar
    |> should equal 0

[<Fact>]
let ``single row with a 1 is 0`` () =
    let input = """10100"""

    input
    |> calculateScrubberScalar
    |> should equal 0

//[<Fact>]
//let ``single row with a 1 is 0`` () =
//    let input = """00100
//11110
//"""
//
//    input
//    |> calculateScrubberScalar
//    |> should equal 0