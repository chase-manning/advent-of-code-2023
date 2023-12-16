module Day16

open System

type Mirrors = char List List

type Direction =
    | Up
    | Down
    | Left
    | Right

type Visit =
    { x: int; y: int; direction: Direction }

let printMirrors (mirrors: Mirrors) (visited: Visit List) =
    mirrors
    |> List.mapi (fun y row ->
        row
        |> List.mapi (fun x c ->
            if visited |> List.exists (fun v -> x = v.x && y = v.y) then
                '#'
            else
                c))
    |> List.map (fun s -> s |> List.map string |> String.concat "")
    |> String.concat "\n"
    |> printfn "%s"

let getToVisit (mirrors: Mirrors) (visit: Visit) =
    let x = visit.x
    let y = visit.y
    let direction = visit.direction

    if x < 0 || x >= mirrors.[0].Length || y < 0 || y >= mirrors.Length then
        []
    else
        let current = mirrors.[y].[x]

        if current = '.' then
            match direction with
            | Up -> [ { x = x; y = y - 1; direction = Up } ]
            | Down -> [ { x = x; y = y + 1; direction = Down } ]
            | Left -> [ { x = x - 1; y = y; direction = Left } ]
            | Right -> [ { x = x + 1; y = y; direction = Right } ]
        else if current = '/' then
            match direction with
            | Up -> [ { x = x + 1; y = y; direction = Right } ]
            | Down -> [ { x = x - 1; y = y; direction = Left } ]
            | Left -> [ { x = x; y = y + 1; direction = Down } ]
            | Right -> [ { x = x; y = y - 1; direction = Up } ]
        else if current = '\\' then
            match direction with
            | Up -> [ { x = x - 1; y = y; direction = Left } ]
            | Down -> [ { x = x + 1; y = y; direction = Right } ]
            | Left -> [ { x = x; y = y - 1; direction = Up } ]
            | Right -> [ { x = x; y = y + 1; direction = Down } ]
        else if current = '-' then
            match direction with
            | Up ->
                [ { x = x - 1; y = y; direction = Left }
                  { x = x + 1; y = y; direction = Right } ]
            | Down ->
                [ { x = x - 1; y = y; direction = Left }
                  { x = x + 1; y = y; direction = Right } ]
            | Left -> [ { x = x - 1; y = y; direction = Left } ]
            | Right -> [ { x = x + 1; y = y; direction = Right } ]
        else if current = '|' then
            match direction with
            | Up -> [ { x = x; y = y - 1; direction = Up } ]
            | Down -> [ { x = x; y = y + 1; direction = Down } ]
            | Left -> [ { x = x; y = y - 1; direction = Up }; { x = x; y = y + 1; direction = Down } ]
            | Right -> [ { x = x; y = y - 1; direction = Up }; { x = x; y = y + 1; direction = Down } ]
        else
            raise (Exception "Unexpected token")

let getVisits (mirrors: Mirrors) : Visit List =
    let rec run (visited: Visit List) (toVisit: Visit List) : Visit List =
        // printMirrors mirrors visited

        if toVisit.Length = 0 then
            visited
        else
            let visit = toVisit.[toVisit.Length - 1]
            let newToVisit = toVisit |> List.rev |> List.tail |> List.rev

            if
                visit.x < 0
                || visit.x >= mirrors.[0].Length
                || visit.y < 0
                || visit.y >= mirrors.Length
            then
                run visited newToVisit
            else if visited |> List.exists (fun v -> v = visit) then
                run visited newToVisit
            else
                run (visited @ [ visit ]) (newToVisit @ getToVisit mirrors visit)

    run [] [ { x = 0; y = 0; direction = Right } ]

let getEnergized (visited: Visit List) : int =
    visited |> List.map (fun v -> (v.x, v.y)) |> List.distinct |> List.length

let getMirrors (line: string List) : Mirrors =
    line |> List.map (fun s -> s.ToCharArray() |> Array.toList)

let part1 (input: string List) =
    let mirrors = getMirrors input
    let visited = getVisits mirrors
    let energized = getEnergized visited
    energized |> string




let part2 (input: string List) = "meow"
