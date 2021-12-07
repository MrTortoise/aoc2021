module Tests

open System
open Xunit
open FsUnit.Xunit

type Board =
    { Cells: list<int>
      Matched: list<int> }

type GameState =
    { Moves: list<int>
      Boards: list<Board> }

let offset = 2
let boardSize = 5
let gap = 1

let exampleInput =
    """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"""

let stringToStringList (input: string) =
    input.Trim().Split("\n")
    |> Array.map (fun i -> i.Trim())
    |> Seq.toList

let parseMoves (input: list<string>) =
    input.[0]
        .Split([| "," |], StringSplitOptions.None)
    |> Array.map int

let replace source index item =
    source
    |> List.mapi (fun i s -> if (i = index) then item else s)

let rec parseLine (gameState: GameState) (lines: list<string>) index : GameState =
    let parseFirstLine (l: string) tail : GameState =
        let moves =
            l.Split([| "," |], StringSplitOptions.None)
            |> Array.toList
            |> List.map int

        let state = { gameState with Moves = moves }
        parseLine state tail 1

    let parseBoardLine (line: string) tail : GameState =
        let numbers =
            line.Split(" ", StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList
            |> List.map (fun i -> int (i.Trim()))

        let boardRow = (index - 2) % 6
        if (boardRow = 0) then
            let boards =
                { Cells = numbers; Matched = [] } :: gameState.Boards 

            let state = { gameState with Boards = boards }
            parseLine state tail (index + 1)
        else
            let boardIndexFront = (index - 2) / 6
            let boardIndex = gameState.Boards.GetReverseIndex(0, boardIndexFront)
            let board = gameState.Boards.[boardIndex]
            let cells = List.append board.Cells numbers
            let newBoard = { board with Cells = cells }
            let boards = replace gameState.Boards boardIndex newBoard
            let state = {gameState with Boards = boards}
            parseLine state tail (index+1)

    match lines with
    | [] -> gameState
    | l :: tail ->
        if index = 0 then
            parseFirstLine l tail
        else if l = "" then
            parseLine gameState tail (index + 1)
        else
            parseBoardLine l tail
            
let parseInput input =
    let lines = input |> stringToStringList
    let gameState = { Moves = []; Boards = [] }

    parseLine gameState lines 0



[<Fact>]
let ``can parse a board`` () =
    let testInput =
        """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19"""

    let game = parseInput testInput
    game.Boards.Length |> should equal 1
    game.Boards.[0].Cells.[0] |> should equal 22
    game.Boards.[0].Cells.Length |> should equal 25
    game.Boards.[0].Cells.[24] |> should equal 19
    game.Moves.Length |> should equal 27
    game.Moves.[0] |> should equal 7
    game.Moves.[26] |> should equal 1

[<Fact>]
let ``Parse exmaple boards and do some basic asserts`` () =
    let game = parseInput exampleInput
    game.Boards.Length |> should equal 3
    game.Boards.[1].Cells.[5] |> should equal 9
