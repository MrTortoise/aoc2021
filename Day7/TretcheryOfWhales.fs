module TretcheryOfWhales

open System.IO
open Xunit
open FsUnit.Xunit

type Position = int
type Quantity = int
type Cost = int
type Range = { Min: Position; Max: Position }

type Point =
    { Position: Position
      Quantity: Quantity }

type CostByPosition = { Position: Position; Cost: Cost }

let buildCosts range =
    let length = range.Max - range.Min
    let costs = Array.zeroCreate (length + 1)

    for i in 1 .. length do
        costs.[i] <- i

    costs

let buildIncreasingCosts range =
    let length = range.Max - range.Min
    let costs = Array.zeroCreate (length + 1)

    for i in 1 .. length do
        costs.[i] <- costs.[i - 1] + i

    costs

let parseToPoints (str: string) =
    str.Split(',')
    |> Array.groupBy id
    |> Array.map
        (fun (a, b) ->
            { Position = int a
              Quantity = b |> Array.length })
    |> Array.sortBy (fun i -> i.Position)

let pointToCost (costLookup: int []) positionToEvaluate (point: Point) =
    let distance =
        if positionToEvaluate > point.Position then
            (positionToEvaluate - point.Position)
        else if positionToEvaluate = point.Position then
            0
        else
            (point.Position - positionToEvaluate)

    { Position = point.Position
      Cost = point.Quantity * costLookup.[distance] }

let costPointsAt (costLookup: int []) position points =
    points
    |> Array.map (pointToCost costLookup position)

let toTotalCost (costs: CostByPosition []) = costs |> Array.sumBy (fun i -> i.Cost)

let buildRange (costByPosition: Point []) : Range =
    { Min = costByPosition.[0].Position
      Max =
          costByPosition.[costByPosition.Length - 1]
              .Position }


let rec getTotalCostInt costLookup maxPos points currentPosition accumulatedCostsByPosition =
    if (currentPosition > maxPos) then
        accumulatedCostsByPosition
    else
        let currentTotal =
            points
            |> costPointsAt costLookup currentPosition
            |> toTotalCost

        getTotalCostInt
            costLookup
            maxPos
            points
            (currentPosition + 1)
            ({ Position = currentPosition
               Cost = currentTotal }
             :: accumulatedCostsByPosition)
                
let getTotalCostByPosition (points: Point []) =
    let range = buildRange points
    let costLookup = buildCosts range

    getTotalCostInt costLookup range.Max points range.Min list.Empty
    |> List.sortBy (fun i -> i.Position)

let getTotalCostByIncreasingPosition (points: Point []) =
    let range = buildRange points
    let costLookup = buildIncreasingCosts range

    getTotalCostInt costLookup range.Max points range.Min list.Empty
    |> List.sortBy (fun i -> i.Position)

[<Fact>]
let ``Find the shortest sum of differences value`` () =
    "16,1,2,0,4,2,7,1,2,14"
    |> parseToPoints
    |> should
        equal
        [| { Position = 0; Quantity = 1 }
           { Position = 1; Quantity = 2 }
           { Position = 2; Quantity = 3 }
           { Position = 4; Quantity = 1 }
           { Position = 7; Quantity = 1 }
           { Position = 14; Quantity = 1 }
           { Position = 16; Quantity = 1 } |]

[<Fact>]
let ``point to cost`` () =
    { Position = 3; Quantity = 5 }
    |> pointToCost (buildCosts { Min = 0; Max = 10 }) 5
    |> should equal { Position = 3; Cost = 10 }

[<Fact>]
let ``find range from CostByPosition`` () =
    [| { Position = 0; Quantity = 1 }
       { Position = 1; Quantity = 2 }
       { Position = 2; Quantity = 3 }
       { Position = 4; Quantity = 1 }
       { Position = 7; Quantity = 1 }
       { Position = 14; Quantity = 1 }
       { Position = 16; Quantity = 1 } |]
    |> buildRange
    |> should equal { Min = 0; Max = 16 }

[<Fact>]
let ``calculate cost for position 0 for all positions`` () =
    [| { Position = 0; Quantity = 1 }
       { Position = 1; Quantity = 2 }
       { Position = 2; Quantity = 3 }
       { Position = 4; Quantity = 1 }
       { Position = 7; Quantity = 1 }
       { Position = 14; Quantity = 1 }
       { Position = 16; Quantity = 1 } |]
    |> costPointsAt (buildCosts { Min = 0; Max = 16 }) 4
    |> should
        equal
        [| { Position = 0; Cost = 4 }
           { Position = 1; Cost = 6 }
           { Position = 2; Cost = 6 }
           { Position = 4; Cost = 0 }
           { Position = 7; Cost = 3 }
           { Position = 14; Cost = 10 }
           { Position = 16; Cost = 12 } |]

[<Fact>]
let ``total cost`` () =
    [| { Position = 0; Cost = 4 }
       { Position = 1; Cost = 6 }
       { Position = 2; Cost = 6 }
       { Position = 4; Cost = 0 }
       { Position = 7; Cost = 3 }
       { Position = 14; Cost = 10 }
       { Position = 16; Cost = 12 } |]
    |> toTotalCost
    |> should equal 41

[<Fact>]
let ``Total costs by position`` () =
    [| { Position = 0; Quantity = 1 }
       { Position = 1; Quantity = 2 }
       { Position = 2; Quantity = 3 }
       { Position = 4; Quantity = 1 }
       { Position = 7; Quantity = 1 }
       { Position = 14; Quantity = 1 }
       { Position = 16; Quantity = 1 } |]
    |> getTotalCostByPosition
    |> List.sortBy (fun i -> i.Position)
    |> should
        equal
        [ { Position = 0; Cost = 49 }
          { Position = 1; Cost = 41 }
          { Position = 2; Cost = 37 }
          { Position = 3; Cost = 39 }
          { Position = 4; Cost = 41 }
          { Position = 5; Cost = 45 }
          { Position = 6; Cost = 49 }
          { Position = 7; Cost = 53 }
          { Position = 8; Cost = 59 }
          { Position = 9; Cost = 65 }
          { Position = 10; Cost = 71 }
          { Position = 11; Cost = 77 }
          { Position = 12; Cost = 83 }
          { Position = 13; Cost = 89 }
          { Position = 14; Cost = 95 }
          { Position = 15; Cost = 103 }
          { Position = 16; Cost = 111 } ]

[<Fact>]
let ``Get the lowest cost by position`` () =
    [| { Position = 0; Quantity = 1 }
       { Position = 1; Quantity = 2 }
       { Position = 2; Quantity = 3 }
       { Position = 4; Quantity = 1 }
       { Position = 7; Quantity = 1 }
       { Position = 14; Quantity = 1 }
       { Position = 16; Quantity = 1 } |]
    |> getTotalCostByPosition
    |> List.minBy (fun i -> i.Cost)
    |> should equal { Position = 2; Cost = 37 }

[<Fact>]
let ``find cost position of test data`` () =
    File.ReadAllText("Day7Data.txt")
    |> parseToPoints
    |> getTotalCostByPosition
    |> List.minBy (fun i -> i.Cost)
    |> should equal { Position = 361; Cost = 354129 }


// Part 2
[<Fact>]
let ``example data with increasing cost over distance`` () =
    "16,1,2,0,4,2,7,1,2,14"
    |> parseToPoints
    |> getTotalCostByIncreasingPosition
    |> should
        equal
        [ { Position = 0; Cost = 290 }
          { Position = 1; Cost = 242 }
          { Position = 2; Cost = 206 }
          { Position = 3; Cost = 183 }
          { Position = 4; Cost = 170 }
          { Position = 5; Cost = 168 }
          { Position = 6; Cost = 176 }
          { Position = 7; Cost = 194 }
          { Position = 8; Cost = 223 }
          { Position = 9; Cost = 262 }
          { Position = 10; Cost = 311 }
          { Position = 11; Cost = 370 }
          { Position = 12; Cost = 439 }
          { Position = 13; Cost = 518 }
          { Position = 14; Cost = 607 }
          { Position = 15; Cost = 707 }
          { Position = 16; Cost = 817 } ]

[<Fact>]
let ``example data with increasing cost over distance total`` () =
    "16,1,2,0,4,2,7,1,2,14"
    |> parseToPoints
    |> getTotalCostByIncreasingPosition
    |> List.minBy (fun i -> i.Cost)
    |> should equal { Position = 5; Cost = 168 }

[<Fact>]
let ``100 distance should cost 5050`` () =
    let costLookup =
        { Min = 0; Max = 100 } |> buildIncreasingCosts

    costLookup.[100] |> should equal 5050

[<Fact>]
let ``find cost position of test data increasing`` () =
    File.ReadAllText("Day7Data.txt")
    |> parseToPoints
    |> getTotalCostByIncreasingPosition
    |> List.minBy (fun i -> i.Cost)
    |> should equal { Position = 494; Cost = 98905973 }
