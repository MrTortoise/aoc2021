module Day1.IncreasesOfTotalSlidingWindow

open Xunit
open FsUnit.Xunit


[<Fact>]
let ``increases of sum of sliding window of [199;200;208;210;200;207;240;269;260;263] item is 5`` () =
    5 |> should equal 5