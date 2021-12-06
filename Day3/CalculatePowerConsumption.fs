module Day3.CalculatePowerConsumption

open System
open Xunit
open FsUnit.Xunit

let gamma line = Convert.ToInt32(line, 2)

let epsilon (line: string) =
    let ep =
        Seq.toList line
        |> Seq.map (fun i -> if i = '0' then '1' else '0')
        |> Seq.toArray
        |> String

    Convert.ToInt32(ep, 2)

let calculatePower line = gamma line * epsilon line

type Signals =
    { i: int
      j: int
      k: int
      l: int
      m: int
      midpoint: int}

let addSignalToExisting(state: Signals) (item: string) =
    let i = if item.[0] = '1' then 1 else 0
    let j = if item.[1] = '1' then 1 else 0
    let k = if item.[2] = '1' then 1 else 0
    let l = if item.[3] = '1' then 1 else 0
    let m = if item.[4] = '1' then 1 else 0

    { state with
          i = state.i + i
          j = state.j + j
          k = state.k + k
          l = state.l + l
          m = state.m + m }
    
let buildBinaryString (signals: Signals) =
    let value total = if (total > signals.midpoint) then "1" else "0"
    
    value signals.i + value signals.j + value signals.k + value signals.l + value signals.m
    

let findModalInput (input: string) =
    let lines = input.Trim().Split("\n")
    let length = lines.Length

    lines
    |> Array.fold addSignalToExisting { i = 0; j = 0; k = 0; l = 0; m = 0; midpoint = length/2 }
    |> buildBinaryString

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
