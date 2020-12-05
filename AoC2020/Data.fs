module AoC2020.Data

let readFullText path =
    System.IO.File.ReadAllText path

let readLines path =
    System.IO.File.ReadAllLines path
    |> List.ofArray

let readLinesAs f path =
    readLines path
    |> List.map f

let readInts path =
    readLinesAs int path
