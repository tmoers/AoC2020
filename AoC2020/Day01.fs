module AoC2020.Day01

let private findPairs predicate numbers =
    List.allPairs numbers numbers
    |> List.filter (fun (a, b) -> predicate a b)
    |> List.filter (fun (a, b) -> a <> b)

let tryFindFirstSumComponents2 sum numbers =
    findPairs (fun a b -> a + b = sum) numbers
    |> List.tryHead

let tryFindFirstSumComponents3 sum numbers =
    let min = List.min numbers
    findPairs (fun a b -> a + b + min <= sum) numbers
    |> List.allPairs numbers
    |> List.map (fun (a, (b, c)) -> a, b, c)
    |> List.filter (fun (a, b, c) -> a + b + c = sum)
    |> List.filter (fun (a, b, c) -> a <> b && a <> c && b <> c)
    |> List.tryHead

let solveA resourcesPath =
    Data.readInts (System.IO.Path.Join(resourcesPath, "day01.input.txt"))
    |> tryFindFirstSumComponents2 2020
    |> Option.map (fun (a, b) -> a * b)

let solveB resourcesPath =
    Data.readInts (System.IO.Path.Join(resourcesPath, "day01.input.txt"))
    |> tryFindFirstSumComponents3 2020
    |> Option.map (fun (a, b, c) -> a * b * c)