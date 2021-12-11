module Day5.HydroThermal

open System
open Xunit
open FsUnit.Xunit

[<CustomComparison; CustomEquality>]
type Point =
    { X: int
      Y: int }
    interface IEquatable<Point> with
        member this.Equals other =
            other.X.Equals this.X && other.Y.Equals this.Y

    override this.Equals other =
        match other with
        | :? Point as p -> (this :> IEquatable<_>).Equals p
        | _ -> false

    override this.GetHashCode() = HashCode.Combine(this.X, this.Y)

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? Point as p -> (this :> IComparable<_>).CompareTo p
            | _ -> -1

    interface IComparable<Point> with
        member this.CompareTo other =
            let xCompare = this.X.CompareTo other.X

            if (xCompare = 0) then
                this.Y.CompareTo other.Y
            else
                xCompare

type Line = { Start: Point; End: Point }

let stringToStringList (input: string) =
    input.Trim().Split("\n")
    |> Array.map (fun i -> i.Trim())
    |> Seq.toList

let parsePoint (input: string) =
    let parts = input.Split ","

    { X = int parts.[0]; Y = int parts.[1] }

let parseVentLine (line: string) =
    let points =
        line.Split "->"
        |> Array.map (fun s -> s.Trim())
        |> Array.map parsePoint
        |> Array.sort

    { Start = points.[0]; End = points.[1] }


let ventLineToPointList (vent: Line) : list<Point> =
    
    
    let rec addLineToVentList (vents: Line) l =
        if (vents.Start >= vents.End) then
            vents.Start :: l
        else
            let restOfLine =
                { Start =
                      { X =
                            if (vents.Start.X = vents.End.X) then
                                vents.Start.X
                            else
                                (vents.Start.X + 1)
                        Y =
                            if (vents.Start.Y = vents.End.Y) then
                                vents.Start.Y
                            else
                                (vents.Start.Y + 1) }
                  End = vents.End }

            addLineToVentList restOfLine (vents.Start :: l)

    if (vent.Start.X = vent.End.X || vent.Start.Y = vent.End.Y) then
        List.empty |> addLineToVentList vent
    else
        []

let parseVents (input: string) =
    input
    |> stringToStringList
    |> List.map parseVentLine
    |> List.map ventLineToPointList
    |> List.concat
    |> List.sort

let toScoredVents pointList =
    pointList
    |> List.groupBy id
    |> List.map (fun (a, b) -> (a, List.length b))

let filterOutNonOverlappingVentLines pointScoreTupleList =
    pointScoreTupleList
    |> List.filter (fun (_, number) -> number > 1)
    |> List.map fst

[<Fact>]
let ``points with lower x are less than`` () =
    [ { X = 1; Y = 0 }; { X = 0; Y = 0 } ]
    |> List.sort
    |> should equal [ { X = 0; Y = 0 }; { X = 1; Y = 0 } ]

[<Fact>]
let ``points with equal x compare y`` () =
    [ { X = 1; Y = 1 }; { X = 1; Y = 0 } ]
    |> List.sort
    |> should equal [ { X = 1; Y = 0 }; { X = 1; Y = 1 } ]


[<Fact>]
let ``parse a row into a sorted line`` () =
    let ventLine = "2,2 -> 2,1" |> parseVentLine

    ventLine
    |> should
        equal
        { Start = { X = 2; Y = 1 }
          End = { X = 2; Y = 2 } }


[<Fact>]
let ``parse a sorted line into a point list`` () =
    { Start = { X = 2; Y = 1 }
      End = { X = 2; Y = 2 } }
    |> ventLineToPointList
    |> List.sort
    |> should equal [ { X = 2; Y = 1 }; { X = 2; Y = 2 } ]

[<Fact>]
let ``parse a sorted line into a point list horizontal`` () =
    { Start = { X = 0; Y = 2 }
      End = { X = 2; Y = 2 } }
    |> ventLineToPointList
    |> List.sort
    |> should
        equal
        [ { X = 0; Y = 2 }
          { X = 1; Y = 2 }
          { X = 2; Y = 2 } ]

[<Fact>]
let ``parse a sorted line into a point list diagonal`` () =
    { Start = { X = 0; Y = 1 }
      End = { X = 2; Y = 3 } }
    |> ventLineToPointList
    |> List.sort
    |> should
        equal
        [ { X = 0; Y = 1 }
          { X = 1; Y = 2 }
          { X = 2; Y = 3 } ]

[<Fact>]
let ``parse 2 rows into point lists`` () =
    let ventLine =
        """2,2 -> 2,1
1,2->2,2
"""
        |> parseVents

    ventLine
    |> should
        equal
        [ { X = 1; Y = 2 }
          { X = 2; Y = 1 }
          { X = 2; Y = 2 }
          { X = 2; Y = 2 } ]

[<Fact>]
let ``point lists into list that counts dupes`` () =
    [ { X = 1; Y = 2 }
      { X = 2; Y = 1 }
      { X = 2; Y = 2 }
      { X = 2; Y = 2 } ]
    |> toScoredVents
    |> should
        equal
        [ ({ X = 1; Y = 2 }, 1)
          ({ X = 2; Y = 1 }, 1)
          ({ X = 2; Y = 2 }, 2) ]


[<Fact>]
let ``take scored list of vents and return only those with score > 1`` () =
    [ ({ X = 1; Y = 2 }, 1)
      ({ X = 2; Y = 1 }, 1)
      ({ X = 2; Y = 2 }, 2) ]
    |> filterOutNonOverlappingVentLines
    |> should equal [ { X = 2; Y = 2 } ]

[<Fact>]
let ``example data results in expected results`` () =
    let exampleData =
        """0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"""

    exampleData
    |> parseVents
    |> toScoredVents
    |> filterOutNonOverlappingVentLines
    |> List.length
    |> should equal 5
