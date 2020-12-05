module AoC2020.Day02

open FParsec

type PasswordPolicy =
    { MinCount : int
      MaxCount : int
      Letter : char
      Password : string }

let parse (line : string) =
    let parser = pint32 .>> pchar '-' .>>. pint32 .>> spaces1 .>>. anyChar .>> pchar ':' .>> spaces1 .>>. restOfLine false
    match run parser line with
    | Success ((((min, max), ch), pwd), _, _) ->
        { MinCount = min
          MaxCount = max
          Letter = ch
          Password = pwd }
        |> Result.Ok
    | Failure (_, e, _) -> Result.Error (sprintf "Invalid password policy line: %O" e)

let isValidOccordingToOldCompany policy =
    let count =
        policy.Password
        |> Seq.filter ((=) policy.Letter)
        |> Seq.length
    policy.MinCount <= count && count <= policy.MaxCount

let isValidOccordingToNewCompany policy =
    OptionComputationExpression.optional {
        let chars = policy.Password.ToCharArray()
        let! char1 = chars |> Array.tryItem (policy.MinCount - 1)
        let! char2 = chars |> Array.tryItem (policy.MaxCount - 1)

        return (char1 = policy.Letter) <> (char2 = policy.Letter)
    }
    |> Option.defaultValue false

let solveA resourcesPath =
    Data.readLinesAs parse (System.IO.Path.Join(resourcesPath, "day02.input.txt"))
    |> List.choose Option.ofResult
    |> List.filter isValidOccordingToOldCompany
    |> List.length

let solveB resourcesPath =
    Data.readLinesAs parse (System.IO.Path.Join(resourcesPath, "day02.input.txt"))
    |> List.choose Option.ofResult
    |> List.filter isValidOccordingToNewCompany
    |> List.length
