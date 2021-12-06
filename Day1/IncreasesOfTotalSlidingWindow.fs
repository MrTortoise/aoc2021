module Day1.IncreasesOfTotalSlidingWindow

open System.IO
open Xunit
open FsUnit.Xunit
open Day1.SlidingWindow
open Day1.CountIncreases



let increases list =
    list |> averageSlidingWindow |> countIncreases


[<Fact>]
let ``increases of sum of sliding window of [199;200;208;210;200;207;240;269;260;263] item is 5`` () =
    let data =
        increases [ 199
                    200
                    208
                    210
                    200
                    207
                    240
                    269
                    260
                    263 ]

    data |> should equal 5


[<Fact>]
let ``increases of the given data should be ...`` () =
    File.ReadLines("Day1Data.txt")
    |> Seq.map (int)
    |> Seq.toList
    |> increases
    |> should equal  1575
    
