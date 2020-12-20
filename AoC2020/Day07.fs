module AoC2020.Day07

open FParsec

type Color = Color of string

type Bag =
    { BagColor : Color
      Capacities : (int * Color) list }

module Color =
    let unwrap (Color v) = v
    let toString = unwrap
    let shinyGold = Color "shiny gold"

module Bag =
    let toString bag =
        Color.toString bag.BagColor



let parse (ruleLine : string) =
    let color =
        many1Satisfy isAsciiLower .>> pchar ' ' .>>. many1Satisfy isAsciiLower |>> fun (s1, s2) -> sprintf "%s %s" s1 s2 |> Color
    let capacity =
        pint64 |>> int .>> pchar ' ' .>>. color .>> pchar ' ' .>> pstring "bag" .>> opt (pchar 's')
    let nothing =
        pstring "no other bags" |>> fun _ -> []
    let capacities =
        sepBy1 capacity (pstring ", ") <|> nothing
    let parser =
        color .>> pchar ' ' .>> pstring "bags contain" .>> pchar ' ' .>>. capacities .>> pchar '.'
        |>> fun (bagColor, capacities) -> { BagColor = bagColor; Capacities = capacities }
    match run parser ruleLine with
    | Success (r, _, _) -> Result.Ok r
    | Failure (_, e, _) -> Result.Error (sprintf "Invalid rule: %O" e)

let canContain wantedColor bag =
    bag.Capacities
    |> List.exists (fun (q, c) -> c = wantedColor)

let tryFindBag allBags wantedColor =
    List.tryFind (fun b -> b.BagColor = wantedColor) allBags
    |> Result.ofOption "Bag not found"

let howEventuallyContains wantedColor bags =
    let rec howEventuallyContains bag =
        if bag.BagColor = wantedColor
        then
            Some []
        else
            bag.Capacities
            |> List.choose (snd >> tryFindBag bags >> Option.ofResult)
            |> List.choose (fun b ->
                howEventuallyContains b
                |> Option.map (fun bs -> bag::b::bs))
            |> List.tryHead

    bags
    |> List.choose howEventuallyContains
    |> List.filter (List.isEmpty >> not)

let countCanEventuallyContain wantedColor bags =
    howEventuallyContains wantedColor bags
    |> List.length

let countContainedBags bags bag =
    let rec countTotalBags bag =
        match bag.Capacities with
        | [] ->
            1
        | _::_ -> 
            let thisBagCount =
                1
            let nestedBagCount =
                bag.Capacities
                |> List.map (fun (q, c) ->
                    match tryFindBag bags c with
                    | Result.Ok b -> q * countTotalBags b
                    | Result.Error _ -> 0)
                |> List.sum
            thisBagCount + nestedBagCount

    countTotalBags bag - 1

let solveA resourcesPath =
    Data.readLinesAs parse (System.IO.Path.Join(resourcesPath, "day07.input.txt"))
    |> Result.sequence
    |> Result.map (countCanEventuallyContain Color.shinyGold)

let solveB resourcesPath =
    Data.readLinesAs parse (System.IO.Path.Join(resourcesPath, "day07.input.txt"))
    |> Result.sequence
    |> Result.bind (fun bags ->
        tryFindBag bags Color.shinyGold
        |> Result.map (countContainedBags bags))