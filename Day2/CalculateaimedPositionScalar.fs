module Day2.CalculateAimedPositionScalar

open Xunit
open FsUnit.Xunit

type Position =
    { Horizontal: int
      Depth: int
      Aim: int }

let processCommand (state: Position) (input: string) =
    let items = input.Split(" ")

    let quant = int (items.[1])
    match items.[0] with
    | "forward" ->
        { state with
              Horizontal = state.Horizontal + quant
              Depth = state.Depth + quant * state.Aim }
    | "down" ->
        { state with
              Aim = state.Aim + quant }
    | "up" ->
        { state with
              Aim = state.Aim - quant }
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
