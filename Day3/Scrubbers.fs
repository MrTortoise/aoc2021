module Day3.Scrubbers

open System
open Xunit
open FsUnit.Xunit

let lifeSupportRating carbon oxygen = carbon * oxygen

let stringToStringList (input: string) =
    input.Trim().Split("\n")
    |> Array.map (fun i -> i.Trim())
    |> Seq.toList

//let co2 (input: string) =
//    input.Trim().Split("\n")
//    |> Array.mapi fun i d ->

let findOxygenRatingOfColumn (index: int) (rows: list<string>) =
    let midPoint = rows.Length / 2

    let total =
        rows
        |> List.map (fun i -> i.[index])
        |> List.filter (fun i -> i = '1')
        |> List.length

    if total >= midPoint then '1' else '0'

let findC02RatingOfColumn (index: int) (rows: list<string>) =
    let midPoint = rows.Length / 2

    let total =
        rows
        |> List.map (fun i -> i.[index])
        |> List.filter (fun i -> i = '1')
        |> List.length

    if total <= midPoint then '1' else '0'

let calculateScrubberScalar input =

    let rowList = input |> stringToStringList

    let width = rowList.[0].Length

    let rec reduceToRating reducer (rows: list<string>) columnIndex : string =
        let ratingOfColumn = reducer columnIndex rows

        let filteredRows =
            rows
            |> List.filter (fun r -> r.[columnIndex] = ratingOfColumn)

        if filteredRows.Length > 1 then
            reduceToRating reducer filteredRows (columnIndex + 1)
        else if filteredRows.Length = 0 then
            "0"
        else
            filteredRows.[0]
        
        

    let oxygenRatingGenerator = reduceToRating findOxygenRatingOfColumn

    let oxygenGeneratorRating =
        Convert.ToInt32(oxygenRatingGenerator rowList 0, 2)

    let co2RatingGenerator = reduceToRating findC02RatingOfColumn

    let co2GeneratorRating =
        Convert.ToInt32(co2RatingGenerator rowList 0, 2)

    lifeSupportRating co2GeneratorRating oxygenGeneratorRating


[<Fact>]
let ``Calculate the scrubber scalar`` () = lifeSupportRating 2 2 |> should equal 4

[<Fact>]
let ``oxygen rating at index 0 is 1`` () =
    let input =
        """00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"""

    input
    |> stringToStringList
    |> findOxygenRatingOfColumn (0)
    |> should equal '1'


[<Fact>]
let ``single row with a 0 is 0`` () =
    let input = """00100"""

    input |> calculateScrubberScalar |> should equal 0

[<Fact>]
let ``single row with a 1 is 0`` () =
    let input = """10100"""

    input |> calculateScrubberScalar |> should equal 0

[<Fact>]
let ``2 row with a 1 is 0`` () =
    let input =
        """00100
11110
"""

    input |> calculateScrubberScalar |> should equal 0
