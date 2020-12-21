module AoC2020.Day01.Day08Tests

open Xunit
open Swensen.Unquote
open AoC2020.Day08


let [<Fact>] ``Instructions are parsed correctly`` () =
    test <@ Ok (Acc +99) = parse "acc +99" @>
    test <@ Ok (Jmp -4) = parse "jmp -4" @>
    test <@ Ok (Nop +0) = parse "nop +0" @>
    
let [<Fact>] ``Run works on the given examples`` () =
    let input =
        [ "nop +0"
          "acc +1"
          "jmp +4"
          "acc +3"
          "jmp -3"
          "acc -99"
          "acc +1"
          "jmp -4"
          "acc +6" ]
    let instructions =
        input |> List.map parse |> Result.sequence |> Result.panic id |> Array.ofList
    let result =
        runProgram instructions

    test <@ ProgramState.isInfiniteLoop result @>
    test <@ { Accumulator = 5; ProgramCounter = 4 } = ProgramState.cpuState result @>
