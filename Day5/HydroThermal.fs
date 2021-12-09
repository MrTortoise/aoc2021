module Day5.HydroThermal

open Xunit
open FsUnit.Xunit

type Point = { X: int; Y: int }
type Line = { Start: Point; End: Point }

type Direction =
    | Vertical of Line
    | Horizontal of Line

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

    let isHorizontal p1 p2 = (p1.X = p2.X)

    let lhs = points.[0]
    let rhs = points.[1]

    if (isHorizontal lhs rhs) then
        if lhs.Y >= rhs.Y then
            Horizontal { Start = rhs; End = lhs }
        else
            Horizontal { Start = lhs; End = rhs }
    else if lhs.X >= rhs.X then
        Vertical { Start = rhs; End = lhs }
    else
        Vertical { Start = lhs; End = rhs }

let addVentToMap (vent: Direction) map =
    let addVent (x: int) (y: int) (m: Map<int, Map<int, int>>) =
        let mapWithX =
            if not (Map.containsKey x m) then
                Map.add x Map.empty m
            else
                m

        let xMap = mapWithX.[x]

        let newXMap =
            if Map.containsKey y xMap then
                let newVal = (xMap.[y] + 1)
                Map.add y newVal xMap
            else
                Map.add y 1 xMap

        Map.add x newXMap m



    let addVerticalVents v m =
        let rec addVerticalInner m x currentY upperY =
            if (currentY > upperY) then
                m
            else
                addVerticalInner (addVent x currentY m) x (currentY + 1) upperY

        addVerticalInner m v.Start.X v.Start.Y v.End.Y

    let addHorizontalVents v m =
        let rec addHorizontalInner m y currentX upperX =
            if (currentX > upperX) then
                m
            else
                addHorizontalInner (addVent currentX y m) y (currentX + 1) upperX

        addHorizontalInner m v.Start.Y v.Start.X v.End.X

    match vent with
    | Vertical v -> addVerticalVents v map
    | Horizontal h -> addHorizontalVents h map


let buildMap (vents: list<Direction>) =
    let rec builder map vents =
        match vents with
        | [] -> map
        | vent :: tail -> builder (addVentToMap vent map) tail

    builder Map.empty vents

let parseVents (input: string) =
    input
    |> stringToStringList
    |> List.map parseVentLine
    |> buildMap

let countWithScoreAbove score vents =
    vents
    |> Map.keys
    |> Seq.map
        (fun k ->
            vents.[k]
            |> Map.values
            |> Seq.filter (fun v -> v > score)
            |> Seq.length)
    |> Seq.sum

[<Fact>]
let ``can add a row and count its items`` () =
    "2,2 -> 2,1"
    |> parseVents
    |> countWithScoreAbove 0
    |> should equal 2
