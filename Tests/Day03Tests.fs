module AoC2020.Day01.Day03Tests

open Xunit
open Swensen.Unquote
open AoC2020.Day03


let [<Fact>] ``The example scenario for A results in the given answer`` () =
    let map =
        [ "..##......."
          "#...#...#.."
          ".#....#..#."
          "..#.#...#.#"
          ".#...##..#."
          "..#.##....."
          ".#.#.#....#"
          ".#........#"
          "#.##...#..."
          "#...##....#"
          ".#..#...#.#" ]
        |> asMap

    test <@ 7 = countTrees { Right = 3; Down = 1 } map @>