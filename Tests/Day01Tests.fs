module AoC2020.Day01.Day01Tests

open Xunit
open Swensen.Unquote
open AoC2020.Day01

let [<Fact>] ``findSumComponents works`` () =
    test <@ Some (7, 13) = tryFindFirstSumComponents2 20 [ 1; 2; 3; 5; 7; 11; 13 ] @>
    test <@ None = tryFindFirstSumComponents2 40 [ 1; 2; 3; 5; 7; 11; 13 ] @>