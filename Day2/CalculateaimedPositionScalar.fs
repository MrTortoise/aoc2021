module Day2.CalculateAimedPositionScalar

open System.IO
open Xunit
open FsUnit.Xunit

type Position =
    { Horizontal: int
      Depth: int
      Aim: int }

let processCommand (state: Position) (input: string) =
    let items = input.Split(" ")

    let quantity = int (items.[1])
    match items.[0] with
    | "forward" ->
        { state with
              Horizontal = state.Horizontal + quantity
              Depth = state.Depth + quantity * state.Aim }
    | "down" ->
        { state with
              Aim = state.Aim + quantity }
    | "up" ->
        { state with
              Aim = state.Aim - quantity }
    | _ -> failwith ("invalid command: " + input)

let calculateScalar (position: Position) =
    match position with
    | position when position.Depth = 0 -> position.Horizontal
    | position when position.Horizontal = 0 -> position.Depth
    | position -> position.Horizontal * position.Depth

let calculatePositionScalar (input: string) =
    let lines = input.Trim().Split "\n"

    lines
    |> Array.fold processCommand { Horizontal = 0; Depth = 0; Aim = 0 }
    |> calculateScalar

[<Fact>]
let ``Forward 5 should result in 5`` () =
    calculatePositionScalar "forward 5"
    |> should equal 5

[<Fact>]
let ``Forward 5 down 5 forward 5 should result in 5`` () =
    let input =
        """forward 5
down 5
"""

    calculatePositionScalar input |> should equal 5

[<Fact>]
let ``Forward 5 down 5 should result in 5`` () =
    let input =
        """forward 5
down 5
forward 5
"""

    calculatePositionScalar input |> should equal 250

[<Fact>]
let ``Example  should result in 900`` () =
    let input =
        """forward 5
down 5
forward 8
up 3
down 8
forward 2
"""

    calculatePositionScalar input |> should equal 900

[<Fact>]
let ``increases of the given data should be ...`` () =
    File.ReadAllText("Day2Data.txt")
    |> calculatePositionScalar
    |> should equal 1848454425