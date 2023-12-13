module Day11

open System

let expandUniverseDown (input: string List) : string List =
    let rec run (index: int) (universe: string List) =
        if index = input.Length then
            universe
        else
            let charArray = input.[index].ToCharArray() |> Array.toList

            if charArray |> List.forall (fun c -> c = '.') then
                run (index + 1) (universe @ [ input[index]; input[index] ])
            else
                run (index + 1) (universe @ [ input.[index] ])

    run 0 []

let appendToString (s: string) (c: char List) : string =
    let charArray = s.ToCharArray() |> Array.toList
    let newCharArray = charArray @ c
    newCharArray |> String.Concat

let expandUniverseRight (input: string List) : string List =
    let rec run (index: int) (universe: string List) =
        if index = input[0].Length then
            universe
        else
            let isEmpty = input |> List.forall (fun s -> s[index] = '.')

            if isEmpty then
                run (index + 1) (universe |> List.map (fun s -> appendToString s [ '.'; '.' ]))
            else
                run (index + 1) (universe |> List.mapi (fun i s -> appendToString s [ input[i][index] ]))

    run 0 (input |> List.map (fun _ -> ""))


let expandUniverse (input: string List) : string List =
    expandUniverseDown input |> expandUniverseRight

let getPoints (input: string List) : (int * int) List =
    let rec run (x: int) (y: int) (points: (int * int) List) =
        if y = input.Length then points
        else if x = input.[0].Length then run 0 (y + 1) points
        else if input.[y].[x] = '.' then run (x + 1) y points
        else run (x + 1) y (points @ [ (x, y) ])

    run 0 0 []


let getPairs (points: (int * int) List) : ((int * int) * (int * int)) List =
    let rec run (index: int) (pairs: ((int * int) * (int * int)) List) =
        if index = points.Length then
            pairs
        else
            let (x, y) = points.[index]

            let rec run2 (index2: int) (pairs: ((int * int) * (int * int)) List) =
                if index2 = points.Length then
                    pairs
                else
                    let (x2, y2) = points.[index2]
                    run2 (index2 + 1) (pairs @ [ ((x, y), (x2, y2)) ])

            run (index + 1) (run2 (index + 1) pairs)

    run 0 []

let getDistance ((x, y), (x2, y2)) =
    let xDistance = abs (x - x2)
    let yDistance = abs (y - y2)
    xDistance + yDistance


let part1 (input: string List) =
    let expanded = expandUniverse input
    // expanded |> List.iter (fun s -> printfn "%s" s)
    let points = getPoints expanded
    // points |> List.iter (fun (x, y) -> printfn "%d %d" x y)
    // printfn "%d" points.Length
    let pairs = getPairs points
    // pairs |> List.iter (fun ((x, y), (x2, y2)) -> printfn "%d %d %d %d" x y x2 y2)
    // printfn "%d" pairs.Length
    let distances = pairs |> List.map getDistance
    distances |> List.sum |> string

let part2 (input: string List) = "meow"
