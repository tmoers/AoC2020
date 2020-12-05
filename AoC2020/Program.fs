open AoC2020

let inline printSolution day answer =
    printf "Solution day %s: %A\n" day answer

[<EntryPoint>]
let main argv =
    let resourcesDir = "../../../../resources"

    printSolution "01-a" (Day01.solveA resourcesDir)
    printSolution "01-b" (Day01.solveB resourcesDir)
    printf "\n"

    0 // return an integer exit code
