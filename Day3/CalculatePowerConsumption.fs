module Day3.CalculatePowerConsumption

open System
open Xunit
open FsUnit.Xunit

let gamma line =
       Convert.ToInt32(line, 2)

let epsilon (line : string) =
    let ep =
        Seq.toList line
        |> Seq.map (fun i -> if i = '0' then '1' else '0')
        |> Seq.toArray
        |> String
    
    Convert.ToInt32(ep,2)

let calculatePower line =
    gamma line * epsilon line
         
[<Fact>]
let ``00100 to decimal`` () =
    "00100"
    |> gamma
    |> should equal 4

[<Fact>]
let ``00100 to epsilon `` () =
    "00100"
    |> epsilon
    |> should equal 27

[<Fact>]
let ``get power for 00100`` ()=
    "00100"
    |> calculatePower
    |> should equal 108