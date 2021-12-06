module Day3.CalculatePowerConsumption

open System
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


let addSignalToExisting(state: int[]) (item: string) =
    let value c = if c = '1' then 1 else 0
    
    item
    |> Seq.toArray    
    |> Array.mapi (fun i c -> Array.set state i (state.[i] + value c))
    |> ignore
    
    state
    
let buildBinaryString(midpoint: int) (signals: int[]) =
    let value total = if (total > midpoint) then "1" else "0"
    
    let items =
        signals
        |> Array.map value
    
    String.concat "" items

let findModalInput (input: string) =
    let lines =
        input.Trim().Split("\n")
        |> Array.map (fun i -> i.Trim())
    let length = lines.Length
    let width = lines.[0].Length
    let midpoint = length / 2
    let buildBinaryMidpoint = buildBinaryString(midpoint)
    let emptyArray = Array.zeroCreate(width)

    lines
    |> Array.fold addSignalToExisting emptyArray
    |> buildBinaryMidpoint

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

    input
    |> findModalInput
    |> calculatePower
    |> should equal 198
