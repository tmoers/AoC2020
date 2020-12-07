module AoC2020.Day01.Day05Tests

open Xunit
open Swensen.Unquote
open AoC2020.Day05


let [<Fact>] ``Seat lines are parsed correctly`` () =
    test <@ Ok { Row =  44; Column = 5 } = parse "FBFBBFFRLR" @>
    test <@ Ok { Row =  70; Column = 7 } = parse "BFFFBBFRRR" @>
    test <@ Ok { Row =  14; Column = 7 } = parse "FFFBBBFRRR" @>
    test <@ Ok { Row = 102; Column = 4 } = parse "BBFFBBFRLL" @>

let [<Fact>] ``The seat is is calculated correctly`` () =
    test <@ 357 = seatId { Row =  44; Column = 5 } @>
    test <@ 567 = seatId { Row =  70; Column = 7 } @>
    test <@ 119 = seatId { Row =  14; Column = 7 } @>
    test <@ 820 = seatId { Row = 102; Column = 4 } @>
