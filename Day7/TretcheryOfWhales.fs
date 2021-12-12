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

let parseToPoints (str: string) =
    str.Split(',')
    |> Array.groupBy id
    |> Array.map
        (fun (a, b) ->
            { Position = int a
              Quantity = b |> Array.length })
    |> Array.sortBy (fun i -> i.Position)



let pointToCost positionToEvaluate (point: Point) =
    let cost =
        if positionToEvaluate > point.Position then
            (positionToEvaluate - point.Position)
        else if positionToEvaluate = point.Position then
            0
        else
            (point.Position - positionToEvaluate)

    { Position = point.Position
      Cost = point.Quantity * cost }

let costPointsAt position points =
    points |> Array.map (pointToCost position)

let toTotalCost (costs: CostByPosition []) = costs |> Array.sumBy (fun i -> i.Cost)

let buildRange (costByPosition: Point []) : Range =
    { Min = costByPosition.[0].Position
      Max =
          costByPosition.[costByPosition.Length - 1]
              .Position }


let getTotalCostByPosition (points: Point []) =
    let range = buildRange points

    let rec getTotalCostInt maxPos points currentPosition accumulatedCostsByPosition =
        if (currentPosition > maxPos) then
            accumulatedCostsByPosition
        else
            let currentTotal =
                points
                |> costPointsAt currentPosition
                |> toTotalCost

            getTotalCostInt
                maxPos
                points
                (currentPosition + 1)
                ({ Position = currentPosition
                   Cost = currentTotal }
                 :: accumulatedCostsByPosition)

    getTotalCostInt range.Max points range.Min list.Empty
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
    |> pointToCost 5
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
    |> costPointsAt 4
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
