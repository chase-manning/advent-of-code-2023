module Day23

type Cell =
    | Wall
    | Path
    | Right
    | Down
    | Left
    | Up

type Map = Cell List List

type Pos = { x: int; y: int }

open System

let printMap (map: Map) (path: Pos Set) =
    printfn "%s" ""
    printfn "%s" ""

    for y = 0 to map.Length - 1 do
        let chars =
            map[y]
            |> List.mapi (fun x c ->
                let pos = { x = x; y = y }

                if path.Contains(pos) then
                    'O'
                else
                    match c with
                    | Wall -> '#'
                    | Path -> '.'
                    | Right -> '>'
                    | Left -> '<'
                    | Up -> '^'
                    | Down -> 'v')

        let string = chars |> List.fold (fun (s: string) (c: char) -> s + (c |> string)) ""
        printfn "%s" string


let charToCell (char: char) : Cell =
    match char with
    | '#' -> Wall
    | '.' -> Path
    | '>' -> Right
    | '<' -> Left
    | '^' -> Up
    | 'v' -> Down
    | _ -> failwith "Unknown char"

let getMapRow (input: string) : Cell List =
    input.ToCharArray() |> List.ofArray |> List.map charToCell

let getMap (input: string List) : Map =
    let rec run (index: int) (map: Map) =
        if index = input.Length then
            map
        else
            let newMapRow = getMapRow input[index]
            run (index + 1) (map @ [ newMapRow ])

    run 0 []

let getPaths (map: Map) : Pos Set List =
    let rec run (path: Pos Set) (pos: Pos) : Pos Set List =
        if pos.x < 0 || pos.x >= map[0].Length then
            []
        else if pos.y < 0 || pos.y >= map.Length then
            []
        else
            let cell = map[pos.y][pos.x]

            if cell = Wall then
                []
            else if path.Contains(pos) then
                []
            else
                let newPath = path.Add(pos)

                if pos.y = map.Length - 1 then
                    [ newPath ]
                else if cell = Down then
                    run newPath { x = pos.x; y = pos.y + 1 }
                else if cell = Up then
                    run newPath { x = pos.x; y = pos.y - 1 }
                else if cell = Left then
                    run newPath { x = pos.x - 1; y = pos.y }
                else if cell = Right then
                    run newPath { x = pos.x + 1; y = pos.y }
                else
                    run newPath { x = pos.x - 1; y = pos.y }
                    @ run newPath { x = pos.x; y = pos.y + 1 }
                    @ run newPath { x = pos.x + 1; y = pos.y }
                    @ run newPath { x = pos.x; y = pos.y - 1 }

    run Set.empty { x = 1; y = 0 }


let maxPath (map: Map) : int =
    // let paths = getPaths map

    // for path in paths do
    //     printMap map path
    //     let meow: Pos Set = path
    //     let steps = (meow |> Set.count) - 1
    //     printfn "%s" (steps |> string)

    let meow = getPaths map |> List.map (fun p -> (p |> Set.count) - 1) |> List.max
    meow
// printfn "%A" meow
// 0
// getPaths map |> List.map (fun p -> p |> Set.count) |> List.max


let part1 (input: string List) =
    let map = getMap input
    maxPath map |> string


let part2 (input: string List) = "meow"
