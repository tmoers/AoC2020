module AoC2020.Day03


let asMap (lines : string list) =
    let height = lines.Length
    let width = List.tryHead lines |> Option.map (fun s ->s.Length)|> Option.defaultValue 0
    let map = Array2D.create width height false

    lines
    |> List.iteri (fun y line ->
        line.ToCharArray()
        |> Array.iteri (fun x char ->
            let isTree =
                char = '#'
            map.SetValue(isTree, x, y)))
    
    map

type SlopeStep =
    { Right : int
      Down : int }

type Position =
    { X : int 
      Y : int }

let mapValue map position =
    let x = position.X % (Array2D.length1 map)
    let y = position.Y
    Array2D.get map x y

let countTrees step map =
    let rec recurse position treeCount =
        if position.Y + step.Down < Array2D.length2 map
        then
            let position = 
                { position with
                    X = position.X + step.Right
                    Y = position.Y + step.Down }
            let treeCount =
                if mapValue map position
                then treeCount + 1
                else treeCount + 0
            recurse position treeCount
        else
            treeCount
    recurse { X = 0; Y = 0 } 0

let solveA resourcesPath =
    Data.readLines (System.IO.Path.Join(resourcesPath, "day03.input.txt"))
    |> asMap
    |> countTrees { Right = 3; Down = 1 }

let solveB resourcesPath =
    let map =
        Data.readLines (System.IO.Path.Join(resourcesPath, "day03.input.txt"))
        |> asMap

    [ { Right = 1; Down = 1 }
      { Right = 3; Down = 1 }
      { Right = 5; Down = 1 }
      { Right = 7; Down = 1 }
      { Right = 1; Down = 2 } ]
    |> List.map (fun slope -> countTrees slope map)
    |> List.reduce ((*))
