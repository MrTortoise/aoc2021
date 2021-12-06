module Day3.CalculatePowerConsumption

open System
open System.IO
open Xunit
open FsUnit.Xunit

let gamma line = Convert.ToInt32(line, 2)

let epsilon (line: string) =
    let ep =
        line.Trim()
        |> Seq.toList
        |> Seq.map (fun i -> if i = '0' then '1' else '0')
        |> Seq.toArray
        |> String

    Convert.ToInt32(ep, 2)

let calculatePower line = gamma line * epsilon line


let addSignalToExisting (state: int []) (item: string) =
    let value c = if c = '1' then 1 else 0
    let incrementArrayValue index character =
        Array.set state index (state.[index] + value character)
        
    item
    |> Seq.toArray
    |> Array.mapi incrementArrayValue
    |> ignore

    state

let buildBinaryString (midpoint: int) (signals: int []) =
    let value total = if (total > midpoint) then "1" else "0"

    let items = signals |> Array.map value

    String.concat "" items

let findModalInput (input: string) =
    let lines =
        input.Trim().Split("\n")
        |> Array.map (fun i -> i.Trim())

    let width = lines.[0].Length
    let emptyArray = Array.zeroCreate (width)

    let midpoint = lines.Length / 2

    lines
    |> Array.fold addSignalToExisting emptyArray
    |> buildBinaryString (midpoint)

let powerFromInput (input: string) =
    input |> findModalInput |> calculatePower

[<Fact>]
let ``00100 to decimal`` () = "00100" |> gamma |> should equal 4

[<Fact>]
let ``00100 to epsilon `` () = "00100" |> epsilon |> should equal 27

[<Fact>]
let ``get power for 00100`` () =
    "00100" |> calculatePower |> should equal 108

[<Fact>]
let ``get power for example`` () =
    let input =
        """
00100
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
01010
"""

    powerFromInput input |> should equal 198

[<Fact>]
let ``increases of the given data should be ...`` () =
    File.ReadAllText("Day3Data.txt")
    |> powerFromInput
    |> should equal 1092896
