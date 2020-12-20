module AoC2020.Day06

open FParsec

type PersonResponse = PersonResponse of char list
type GroupResponse = GroupResponse of PersonResponse list

module PersonResponse =
    let unwrap (PersonResponse v) = v

module GroupResponse =
    let unwrap (GroupResponse v) = v

let parse (fullText : string) =
    let personResponse =
        many1 asciiLetter .>> newline |>> PersonResponse
    let groupResponse =
        many1 personResponse |>> GroupResponse
    let parser =
        sepBy1 groupResponse newline .>> eof
    match run parser fullText with
    | Success (r, _, _) -> Result.Ok r
    | Failure (_, e, _) -> Result.Error (sprintf "Invalid input: %O" e)

let countUniqueGroupYesAnswers (GroupResponse groupResponse) =
    groupResponse
    |> List.map PersonResponse.unwrap
    |> List.collect id
    |> List.distinct
    |> List.length

let countCommonGroupYesAnswers (GroupResponse groupResponse) =
    groupResponse
    |> List.map (PersonResponse.unwrap >> Set.ofList)
    |> Set.intersectMany
    |> Set.count

let solveA resourcesPath =
    Data.readFullText (System.IO.Path.Join(resourcesPath, "day06.input.txt"))
    |> parse
    |> Result.map (List.sumBy countUniqueGroupYesAnswers)

let solveB resourcesPath =
    Data.readFullText (System.IO.Path.Join(resourcesPath, "day06.input.txt"))
    |> parse
    |> Result.map (List.sumBy countCommonGroupYesAnswers)
