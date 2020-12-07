module AoC2020.Day05

open FParsec

type Seat =
    { Row : int
      Column : int }

type Partition =
    | LowerHalf
    | UpperHalf

let seatId seat =
    seat.Row * 8 + seat.Column

let ofPartitioning (zones, columns) =
    let rec calculate total partitions =
        match partitions with
        | [] -> total
        | LowerHalf::tail -> calculate total tail
        | UpperHalf::tail -> calculate (total + (1 <<< (List.length partitions - 1))) tail
    { Row = calculate 0 zones
      Column = calculate 0 columns }

let parse (seatLine : string) =
    let zone =
        charReturn 'F' LowerHalf  <|> charReturn 'B' UpperHalf
    let column =
        charReturn 'L' LowerHalf  <|> charReturn 'R' UpperHalf
    let parser =
        (parray 7 zone |>> List.ofArray) .>>. (parray 3 column |>> List.ofArray)
    match run parser seatLine with
    | Success (r, _, _) -> Result.Ok (ofPartitioning r)
    | Failure (_, e, _) -> Result.Error (sprintf "Invalid passport line: %O" e)

let findMissingSeats seats =
    let findMissingSeats rowSeats =
        let fullRowColumns = set [0;1;2;3;4;5;6;7]
        let row = rowSeats |> List.tryHead |> Option.map (fun s -> s.Row)
        let columns = List.map (fun s -> s.Column) rowSeats |> Set.ofList
        let missingColumns = Set.difference fullRowColumns columns |> List.ofSeq
        missingColumns |> List.choose (fun c -> row |> Option.map (fun r -> { Row = r; Column = c}))

    let seats =
        seats
        |> List.sortBy (fun s -> s.Row, s.Column)
    let firstRow = 
        List.tryHead seats
        |> Option.map (fun s -> s.Row)
        |> Option.defaultValue 0
    let lastRow =
        List.tryLast seats
        |> Option.map (fun s -> s.Row)
        |> Option.defaultValue 0

    seats
    |> List.filter (fun s -> s.Row <> firstRow && s.Row <> lastRow)
    |> List.groupBy (fun s -> s.Row)
    |> List.map snd
    |> List.filter (fun rowSeats -> rowSeats.Length <> 8)
    |> List.collect (fun rowSeats -> findMissingSeats rowSeats)


let solveA resourcesPath =
    Data.readLines (System.IO.Path.Join(resourcesPath, "day05.input.txt"))
    |> List.choose (parse >> Option.ofResult)
    |> List.maxBy seatId
    |> fun seat -> seat, seatId seat

let solveB resourcesPath =
    Data.readLines (System.IO.Path.Join(resourcesPath, "day05.input.txt"))
    |> List.choose (parse >> Option.ofResult)
    |> findMissingSeats
    |> List.map (fun seat -> seat, seatId seat)
