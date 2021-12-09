module Day4.Bingo

open System
open System.IO
open Xunit
open FsUnit.Xunit

type Board =
    { Cells: list<int>
      Matched: list<int> }

type GameState =
    { Moves: list<int>
      Boards: list<Board>
      Score: int }

let offset = 2
let boardSize = 5
let gap = 1

let stringToStringList (input: string) =
    input.Trim().Split("\n")
    |> Array.map (fun i -> i.Trim())
    |> Seq.toList

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
                { Cells = numbers; Matched = [] }
                :: gameState.Boards

            let state = { gameState with Boards = boards }
            parseLine state tail (index + 1)
        else
            let boardIndexFront = (index - 2) / 6

            let boardIndex =
                gameState.Boards.GetReverseIndex(0, boardIndexFront)

            let board = gameState.Boards.[boardIndex]
            let cells = List.append board.Cells numbers
            let newBoard = { board with Cells = cells }

            let boards =
                replace gameState.Boards boardIndex newBoard

            let state = { gameState with Boards = boards }
            parseLine state tail (index + 1)

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
    let gameState = { Moves = []; Boards = []; Score = 0 }

    parseLine gameState lines 0

let applyMoveToBoard move (board: Board) =
    if List.contains move board.Cells then
        { board with
              Matched = move :: board.Matched }
    else
        board

let checkForHorizontalWin (board: Board) : bool =
    let cellMatches row =
        row
        |> List.forall (fun c -> List.contains c board.Matched)

    List.chunkBySize 5 board.Cells
    |> List.exists cellMatches

let checkForVerticalWin (board: Board) =
    let matches = board.Matched
    let cells = Array2D.create 5 5 0

    board.Cells
    |> List.iteri
        (fun i v ->
            let row = i / 5
            let col = i % 5
            cells.[row, col] <- v)

    let rec checkColumn (column: int) (row: int) =
        let matcher i = List.contains cells.[i, column] matches

        match row with
        | 0 -> matcher 0
        | i ->
            if not (matcher i) then
                false
            else
                checkColumn column (i - 1)

    let rec checkAllColumns column =
        match column with
        | 0 -> checkColumn 0 4
        | i ->
            if not (checkColumn i 4) then
                checkAllColumns (column - 1)
            else
                true

    checkAllColumns 4

let scoreBoard (board: Board) =
    board.Cells
    |> List.filter (fun i -> not (List.contains i board.Matched))
    |> List.sum

let isBoardAWinner board =
    let horizontalWin = checkForHorizontalWin board
    let verticalWin = checkForVerticalWin board
    horizontalWin || verticalWin

let rec applyWinState state boards =
    match boards with
    | [] -> state
    | b :: tail ->
        if (isBoardAWinner b) then
            { state with Score = scoreBoard b }
        else
            applyWinState state tail

let runGame (gameState: GameState) : int =
    let rec applyMovesFindWinner (state: GameState) (moves: list<int>) =
        match moves with
        | [] -> 0
        | h :: tail ->
            let moveToApplyToBoard = applyMoveToBoard h

            let boards =
                state.Boards |> List.map moveToApplyToBoard

            let stateWithMove = { state with Boards = boards }
            let stateWithWin = applyWinState stateWithMove boards

            if stateWithWin.Score = 0 then
                applyMovesFindWinner stateWithWin tail
            else
                stateWithWin.Score * h

    applyMovesFindWinner gameState gameState.Moves

let loseGame (gameState: GameState) : int =
    let rec applyMovesToFindLoser (state: GameState) (moves: list<int>) =
        match moves with
        | [] -> failwith "ran out of moves!"
        | h :: tail ->
            let moveToApplyToBoard = applyMoveToBoard h

            let boards =
                state.Boards |> List.map moveToApplyToBoard

            let remainingBoards =
                if boards.Length > 1 then
                    boards
                    |> List.filter (fun b -> not (isBoardAWinner b))
                else
                    boards

            let stateWithBoardsRemoved = { state with Boards = remainingBoards }

            if (stateWithBoardsRemoved.Boards.Length = 1
                && isBoardAWinner stateWithBoardsRemoved.Boards.[0]) then
                scoreBoard stateWithBoardsRemoved.Boards.[0] * h
            else
                applyMovesToFindLoser stateWithBoardsRemoved tail

    applyMovesToFindLoser gameState gameState.Moves

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
let ``Parse example boards and do some basic asserts`` () =
    let game = parseInput exampleInput
    game.Boards.Length |> should equal 3
    game.Boards.[1].Cells.[5] |> should equal 9

[<Fact>]
let ``play through the moves and find a horizontal winner`` () =
    let testInput =
        """22,13,17,0,11

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19""" //237

    let score = parseInput testInput |> runGame

    score |> should equal 2607

[<Fact>]
let ``play through the moves and find a vertical winner`` () =
    let testInput =
        """22,8,21,6,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19""" //242

    let score = parseInput testInput |> runGame
    score |> should equal 242

[<Fact>]
let ``play through the example input and find score`` () =
    parseInput exampleInput
    |> runGame
    |> should equal 4512

[<Fact>]
let ``play through the test input and find score`` () =
    File.ReadAllText("Day4Data.txt")
    |> parseInput
    |> runGame
    |> should equal 10374

[<Fact>]
let ``play through the example input and losing score`` () =
    parseInput exampleInput
    |> loseGame
    |> should equal 1924

[<Fact>]
let ``play through the test input and find losing score`` () =
    File.ReadAllText("Day4Data.txt")
    |> parseInput
    |> loseGame
    |> should equal 10374
