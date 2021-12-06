module Day3.Scrubbers

open Xunit
open FsUnit.Xunit



let scrubUp carbon oxygen = carbon * oxygen

[<Fact>]
let ``Calculate the scrubber scalar`` () =
    scrubUp 2 2 |> should equal 4

//[<Fact>]
//let ``00100 to decimal`` () = "00100" |> gamma |> should equal 4