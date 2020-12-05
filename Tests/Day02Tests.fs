module AoC2020.Day01.Day02Tests

open Xunit
open Swensen.Unquote
open AoC2020.Day02

let makePolicy (min, max) char pwd =
    { MinCount = min
      MaxCount = max
      Letter = char
      Password = pwd }

let [<Fact>] ``parse works`` () =
    test <@ Ok (makePolicy (1, 3) 'a' "abcde") = (parse "1-3 a: abcde") @>


let [<Fact>] ``isValidOccordingToNewCompany works`` () =
    test <@ true = isValidOccordingToNewCompany (makePolicy (1, 3) 'a' "abcde") @>
    test <@ false = isValidOccordingToNewCompany (makePolicy (1, 3) 'b' "cdefg") @>
    test <@ false = isValidOccordingToNewCompany (makePolicy (2, 9) 'c' "ccccccccc") @>

