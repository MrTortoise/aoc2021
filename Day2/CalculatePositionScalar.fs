module Day2.CalculatePositionScalar

open System.IO
open Xunit
open FsUnit.Xunit

type Position = { Horizontal: int; Depth: int }

let processCommand (state: Position) (input: string) =
    let items = input.Split(" ")

    match items.[0] with
    | "forward" ->
        { Horizontal = state.Horizontal + int (items.[1])
          Depth = state.Depth }
    | "down" ->
        { Depth = state.Depth + int (items.[1])
          Horizontal = state.Horizontal }
    | "up" ->
        { Depth = state.Depth - int (items.[1])
          Horizontal = state.Horizontal }
    | _ -> failwith ("invalid command: " + input)

let calculateScalar (position: Position) =
    match position with
    | position when position.Depth = 0 -> position.Horizontal
    | position -> position.Horizontal * position.Depth

let calculatePositionScalar (input: string) =
    let lines = input.Trim().Split "\n"

    lines
    |> Array.fold processCommand { Horizontal = 0; Depth = 0 }
    |> calculateScalar

[<Fact>]
let ``Forward 5 should result in 5`` () =
    calculatePositionScalar "forward 5"
    |> should equal 5

[<Fact>]
let ``Forward 5 down 5 should result in 25`` () =
    let input =
        """forward 5
down 5
"""

    calculatePositionScalar input |> should equal 25

[<Fact>]
let ``f5d5f8 should result in 65`` () =
    let input =
        """forward 5
down 5
forward 8
"""

    calculatePositionScalar input |> should equal 65

[<Fact>]
let ``example data should result in 150`` () =
    let input =
        """
forward 5
down 5
forward 8
up 3
down 8
forward 2
"""

    calculatePositionScalar input |> should equal 150

[<Fact>]
let ``increases of the given data should be ...`` () =
    File.ReadAllText("Day2Data.txt")
    |> calculatePositionScalar
    |> should equal 1561344
