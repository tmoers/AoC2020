module AoC2020.Day01.Day07Tests

open Xunit
open Swensen.Unquote
open AoC2020.Day07

let bag color capacities =
    { BagColor = Color color
      Capacities = capacities |> List.map (fun (amount, color) -> amount, Color color ) }

let [<Fact>] ``Answers are parsed correctly`` () =
    test <@ Ok (bag ("faded blue") []) = parse "faded blue bags contain no other bags." @>
    test <@ Ok (bag ("bright white") [1, ("shiny gold")]) = parse "bright white bags contain 1 shiny gold bag." @>
    test <@ Ok (bag ("light red") [1, ("bright white"); 2, ("muted yellow")]) = parse "light red bags contain 1 bright white bag, 2 muted yellow bags." @>

let [<Fact>] ``Can eventually contain works as expected`` () =
    let input =
        [ "light red bags contain 1 bright white bag, 2 muted yellow bags."
          "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
          "bright white bags contain 1 shiny gold bag."
          "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
          "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
          "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
          "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
          "faded blue bags contain no other bags."
          "dotted black bags contain no other bags." ]
    let bags =
        input |> List.map parse |> Result.sequence |> Result.panic id

    test <@ 4 = countCanEventuallyContain Color.shinyGold bags @>

let [<Fact>] ``Count totoal bags works as expected`` () =
    let input =
        [ "shiny gold bags contain 2 dark red bags."
          "dark red bags contain 2 dark orange bags."
          "dark orange bags contain 2 dark yellow bags."
          "dark yellow bags contain 2 dark green bags."
          "dark green bags contain 2 dark blue bags."
          "dark blue bags contain 2 dark violet bags."
          "dark violet bags contain no other bags." ]
    let bags =
        input |> List.map parse |> Result.sequence |> Result.panic id
    let goldBag =
        tryFindBag bags Color.shinyGold
        |> Result.panic id

    test <@ 126 = countContainedBags bags goldBag @>

let [<Fact>] ``Count totoal bags works as expected2`` () =
    let input =
        [ "light red bags contain 1 bright white bag, 2 muted yellow bags."
          "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
          "bright white bags contain 1 shiny gold bag."
          "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
          "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
          "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
          "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
          "faded blue bags contain no other bags."
          "dotted black bags contain no other bags." ]
    let bags =
        input |> List.map parse |> Result.sequence |> Result.panic id
    let goldBag =
        tryFindBag bags Color.shinyGold
        |> Result.panic id

    test <@ 32 = countContainedBags bags goldBag @>