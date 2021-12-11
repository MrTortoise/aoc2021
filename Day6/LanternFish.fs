module LanternFish

open System.IO
open Xunit
open FsUnit.Xunit

let stepFish school (fish: int) =
    let newFish = (fish - 1)

    if (newFish = -1) then
        6 :: 8 :: school
    else
        newFish :: school

let iterate count (input: string) =
    let rec generate remaining (fish: list<int>) =
        if (remaining = 0) then
            fish
        else
            let newFish = fish |> List.fold (stepFish) []
            generate (remaining - 1) newFish

    let gen0 =
        input.Split(',') |> Array.toList |> List.map int

    generate count gen0


let fishToString (fish: list<int>) =
    fish
    |> List.sort
    |> List.map string
    |> String.concat ","
    
let sortString (str: string) =
    str.Split(',')
    |> Array.map int
    |> Array.sort
    |> Array.map string
    |> String.concat ","

[<Fact>]
let ``sorts strings`` () =
    "2,3,2,0,1"
    |> sortString
    |> should equal "0,1,2,2,3"

[<Fact>]
let ``Example data after 1 iteration`` () =
    "3,4,3,1,2"
    |> iterate 1
    |> fishToString
    |> should equal ("2,3,2,0,1" |> sortString)


[<Fact>]
let ``Example data after 2 iteration`` () =
    "3,4,3,1,2"
    |> iterate 2
    |> fishToString
    |> should equal ("1,2,1,6,0,8" |> sortString)

[<Fact>]
let ``Example data after 3 iteration`` () =
    "3,4,3,1,2"
    |> iterate 3
    |> fishToString
    |> should equal ("0,1,0,5,6,7,8" |> sortString)

[<Fact>]
let ``Example data after 18 iteration`` () =
    "3,4,3,1,2"
    |> iterate 18
    |> fishToString
    |> should equal ("6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8" |> sortString)
    
[<Fact>]
let ``Example data total after 80 iteration`` () =
    "3,4,3,1,2"
    |> iterate 80
    |> List.length
    |> should equal 5934

[<Fact>]
let ``play through the test input and find losing score`` () =
    File.ReadAllText("Day6Data.txt")
    |> iterate 80    
    |> List.length
    |> should equal 385391
