open AoC2020

let inline printSolution day answer =
    printf "Solution day %s: %A\n" day answer

[<EntryPoint>]
let main argv =
    let resourcesDir = "../../../../resources"

    printSolution "01-a" (Day01.solveA resourcesDir)
    printSolution "01-b" (Day01.solveB resourcesDir)
    printf "\n"

    printSolution "02-a" (Day02.solveA resourcesDir)
    printSolution "02-b" (Day02.solveB resourcesDir)
    printf "\n"

    printSolution "03-a" (Day03.solveA resourcesDir)
    printSolution "03-b" (Day03.solveB resourcesDir)
    printf "\n"

    printSolution "04-a" (Day04.solveA resourcesDir)
    printSolution "04-b" (Day04.solveB resourcesDir)
    printf "\n"

    printSolution "05-a" (Day05.solveA resourcesDir)
    printSolution "05-b" (Day05.solveB resourcesDir)
    printf "\n"

    printSolution "06-a" (Day06.solveA resourcesDir)
    printSolution "06-b" (Day06.solveB resourcesDir)
    printf "\n"

    printSolution "07-a" (Day07.solveA resourcesDir)
    printSolution "07-b" (Day07.solveB resourcesDir)
    printf "\n"

    0 // return an integer exit code
