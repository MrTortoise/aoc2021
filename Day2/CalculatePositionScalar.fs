module Day2.CalculatePositionScalar

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
