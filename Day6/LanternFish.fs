module LanternFish

open System.IO
open Xunit
open FsUnit.Xunit

type FishData =
    { Day0: int64
      Day1: int64
      Day2: int64
      Day3: int64
      Day4: int64
      Day5: int64
      Day6: int64
      Day7: int64
      Day8: int64 }

let stepFish school (fish: int) =
    let newFish = (fish - 1)

    if (newFish = -1) then
        6 :: 8 :: school
    else
        newFish :: school

let rec generate remaining (fish: FishData) =
    if (remaining = 0) then
        fish
    else
        let fish0 = fish.Day0

        let fishState =
            { Day0 = fish.Day1
              Day1 = fish.Day2
              Day2 = fish.Day3
              Day3 = fish.Day4
              Day4 = fish.Day5
              Day5 = fish.Day6
              Day6 = fish.Day7 + fish0
              Day7 = fish.Day8
              Day8 = fish0 }

        generate (remaining - 1) fishState

let iterate count (input: string) =
    let gen0 =
        input.Split(',')
        |> Array.map int
        |> Array.groupBy id
        |> Array.map (fun (a, b) -> (int64 a, int64 (Array.length b)))
        |> Array.sortBy fst

    let valOrDefault (array: (int64*int64)[]) index (def:int64) : int64 =
        let item =
            array
            |> Array.filter (fun (a,b) -> a = index)
        if not (Array.isEmpty item) then
            snd item.[0]
        else
            def
            
    let state =
        { Day0 = valOrDefault gen0 0L 0L
          Day1 = valOrDefault gen0 1L 0L
          Day2 = valOrDefault gen0 2L 0L
          Day3 = valOrDefault gen0 3L 0L
          Day4 = valOrDefault gen0 4L 0L
          Day5 = valOrDefault gen0 5L 0L
          Day6 = valOrDefault gen0 6L 0L
          Day7 = valOrDefault gen0 7L 0L
          Day8 = valOrDefault gen0 8L 0L }

    generate count state

let sumFish (fishData: FishData) =
    fishData.Day0
    + fishData.Day1
    + fishData.Day2
    + fishData.Day3
    + fishData.Day4
    + fishData.Day5
    + fishData.Day6
    + fishData.Day7
    + fishData.Day8


[<Fact>]
let ``Example data total after 80 iteration`` () =
    "3,4,3,1,2"
    |> iterate 80
    |> sumFish
    |> should equal 5934L

[<Fact>]
let ``Example data total after 256 iteration`` () =
    let expected : int64 = 26984457539L
    "3,4,3,1,2"
    |> iterate 256
    |> sumFish
    |> should equal expected

[<Fact>]
let ``play through the test input and find losing score`` () =
    File.ReadAllText("Day6Data.txt")
    |> iterate 80
    |> sumFish
    |> should equal 385391L

[<Fact>]
let ``play through the test input for 18 days and find losing score`` () =
    File.ReadAllText("Day6Data.txt")
    |> iterate 80
    |> sumFish
    |> should equal 385391L

[<Fact>]
let ``p2 with 256 days`` () =
    File.ReadAllText("Day6Data.txt")
    |> iterate 256
    |> sumFish
    |> should equal 1728611055389L
