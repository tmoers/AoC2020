module AoC2020.Day04

open FParsec

module private Internals =
    let passportIdRegex = System.Text.RegularExpressions.Regex("\d{9}", System.Text.RegularExpressions.RegexOptions.Compiled)
    let heigthRegex = System.Text.RegularExpressions.Regex("(\d+)(cm|in)", System.Text.RegularExpressions.RegexOptions.Compiled)
    let hairColorRegex = System.Text.RegularExpressions.Regex("#([a-f\d]{6})", System.Text.RegularExpressions.RegexOptions.Compiled)

type Passport =
    { BirthYear : int // byr
      IssueYear : int // iyr
      ExpirationYear : int // eyr
      Height : string // hgt
      HairColor : string // hcl
      EyeColor : string // ecl
      PassportId : string // pid
      CountryId : string option } // cid

let private ofKvs kvs =
    let tryFindValue key =
        kvs
        |> List.tryFind (fun (k, _) -> k = key)
        |> Option.map snd
        |> Result.ofOption (sprintf "Key '%s' not found" key)
    if List.length kvs < 7
    then Result.Error (sprintf "Only %i passport fields" (List.length kvs))
    else 
        result {
            let! byr = tryFindValue "byr" |> Result.map int
            let! iyr = tryFindValue "iyr" |> Result.map int
            let! eyr = tryFindValue "eyr" |> Result.map int
            let! hgt = tryFindValue "hgt"
            let! hcl = tryFindValue "hcl"
            let! ecl = tryFindValue "ecl"
            let! pid = tryFindValue "pid"
            let cid = tryFindValue "cid" |> Option.ofResult

            return
                { BirthYear = byr
                  IssueYear = iyr
                  ExpirationYear = eyr
                  Height = hgt 
                  HairColor = hcl
                  EyeColor = ecl
                  PassportId = pid
                  CountryId = cid }
        }

let validate passport =
    let validatePid v =
        if Internals.passportIdRegex.IsMatch(v)
        then Result.Ok v
        else Result.Error "PassportId is not a nine digit number"
    let validateHairColor v =
        if Internals.hairColorRegex.IsMatch(v)
        then Result.Ok v
        else Result.Error "PassportId is not a nine digit number"
    let validateBetween name min max v =
        if min <= v && v <= max
        then Result.Ok v
        else Result.Error (sprintf "'%s' %i is not in the valid range [%i, %i]" name v min max)
    let validateHeight v =
        let m = Internals.heigthRegex.Match(v)
        if m.Success
        then match m.Groups.[2].Value, int m.Groups.[1].Value with
             | "in", h -> validateBetween "hgt(in)" 59 76 h
             | "cm", h -> validateBetween "hgt(cm)" 150 193 h
             | unit, h -> Result.Error (sprintf "Unknown hgt unit '%s'" unit)
        else Result.Error (sprintf "Unknown hgt '%s'" v)
    let validateEyeColor v =
        match v with
        | "amb" | "blu" | "brn"
        | "gry" | "grn" | "hzl"
        | "oth" -> Result.Ok v
        | _ -> Result.Error (sprintf "Not a valid eye color '%s'" v)
    result {
        let! byr = validateBetween "byr" 1920 2002 passport.BirthYear
        let! iyr = validateBetween "iyr" 2010 2020 passport.IssueYear
        let! eyr = validateBetween "eyr" 2020 2030 passport.ExpirationYear
        let! hgt = validateHeight passport.Height
        let! hcl = validateHairColor passport.HairColor
        let! ecl = validateEyeColor passport.EyeColor
        let! pid = validatePid passport.PassportId
        return passport
    }

let parse (passportLine : string) =
    let keyValueParser =
        anyString 3 .>> pchar ':' .>>. manySatisfy (fun c -> c <> ' ')
    let parser =
        sepBy keyValueParser (pchar ' ')
    match run parser passportLine with
    | Success (kvs, _, _) -> ofKvs kvs
    | Failure (_, e, _) -> Result.Error (sprintf "Invalid passport line: %O" e)

let preprocess lines =
    let join lineParts =
        String.concat " " lineParts
    let combine (fullPassportLines, lineParts) =
        (join lineParts)::fullPassportLines
    lines
    |> List.fold (fun ((fullPassportLines, lineParts) as acc) line ->
        if System.String.IsNullOrWhiteSpace(line)
        then combine acc, []
        else (fullPassportLines, line::lineParts))
        ([], [])
    |> combine
    |> List.rev

let solveA resourcesPath =
    Data.readLines (System.IO.Path.Join(resourcesPath, "day04.input.txt"))
    |> preprocess
    |> List.choose (parse >> Option.ofResult)
    |> List.length

let solveB resourcesPath =
    Data.readLines (System.IO.Path.Join(resourcesPath, "day04.input.txt"))
    |> preprocess
    |> List.choose (parse >> Result.bind validate >> Option.ofResult)
    |> List.length
