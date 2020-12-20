module AoC2020.Day01.Day06Tests

open Xunit
open Swensen.Unquote
open AoC2020.Day06


let [<Fact>] ``Answers are parsed correctly`` () =
    let input =
        [ "abc"
          ""
          "a"
          "b"
          "c"
          ""
          "ab"
          "ac"
          ""
          "a"
          "a"
          "a"
          "a"
          ""
          "b"
          ""
          ]
        |> String.concat "\n"
    let groupResponses =
        parse input

    let thirdGroupResponse =
        GroupResponse
            [ PersonResponse ['a'; 'b']
              PersonResponse ['a'; 'c'] ]
    let lastGroupResponse = 
        GroupResponse
            [ PersonResponse ['b'] ]

    test <@ Ok 5 = (groupResponses |> Result.map List.length) @>
    test <@ Ok thirdGroupResponse = (groupResponses |> Result.map (List.item 2)) @>
    test <@ Ok lastGroupResponse = (groupResponses |> Result.map List.last) @>

let [<Fact>] ``Unique yes answers are counted correctly`` () =
    test <@ 3 = countUniqueGroupYesAnswers (GroupResponse [ PersonResponse ['a'; 'b'; 'c'] ]) @>
    test <@ 3 = countUniqueGroupYesAnswers (GroupResponse [ PersonResponse ['a']; PersonResponse ['b']; PersonResponse ['c'] ]) @>
    test <@ 3 = countUniqueGroupYesAnswers (GroupResponse [ PersonResponse ['a'; 'b']; PersonResponse ['a'; 'c'] ]) @>
    test <@ 1 = countUniqueGroupYesAnswers (GroupResponse [ PersonResponse ['a']; PersonResponse ['a']; PersonResponse ['a'] ]) @>
    test <@ 1 = countUniqueGroupYesAnswers (GroupResponse [ PersonResponse ['a'] ]) @>

let [<Fact>] ``Common yes answers are counted correctly`` () =
    test <@ 3 = countCommonGroupYesAnswers (GroupResponse [ PersonResponse ['a'; 'b'; 'c'] ]) @>
    test <@ 0 = countCommonGroupYesAnswers (GroupResponse [ PersonResponse ['a']; PersonResponse ['b']; PersonResponse ['c'] ]) @>
    test <@ 1 = countCommonGroupYesAnswers (GroupResponse [ PersonResponse ['a'; 'b']; PersonResponse ['a'; 'c'] ]) @>
    test <@ 1 = countCommonGroupYesAnswers (GroupResponse [ PersonResponse ['a']; PersonResponse ['a']; PersonResponse ['a'] ]) @>
    test <@ 1 = countCommonGroupYesAnswers (GroupResponse [ PersonResponse ['a'] ]) @>
