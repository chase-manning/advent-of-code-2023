module Day16

open System

type Mirrors = char List List

type Visit = { x: int; y: int; dir: char }

let inRange x y (mirrors: Mirrors) =
    x >= 0 && x < mirrors.[0].Length && y >= 0 && y < mirrors.Length

let getToVisit mirrors visit =
    let x = visit.x
    let y = visit.y
    let dir = visit.dir

    if not (inRange x y mirrors) then
        []
    else
        let current = mirrors.[y].[x]

        if current = '.' then
            match dir with
            | 'U' -> [ { x = x; y = y - 1; dir = 'U' } ]
            | 'D' -> [ { x = x; y = y + 1; dir = 'D' } ]
            | 'L' -> [ { x = x - 1; y = y; dir = 'L' } ]
            | _ -> [ { x = x + 1; y = y; dir = 'R' } ]
        else if current = '/' then
            match dir with
            | 'U' -> [ { x = x + 1; y = y; dir = 'R' } ]
            | 'D' -> [ { x = x - 1; y = y; dir = 'L' } ]
            | 'L' -> [ { x = x; y = y + 1; dir = 'D' } ]
            | _ -> [ { x = x; y = y - 1; dir = 'U' } ]
        else if current = '\\' then
            match dir with
            | 'U' -> [ { x = x - 1; y = y; dir = 'L' } ]
            | 'D' -> [ { x = x + 1; y = y; dir = 'R' } ]
            | 'L' -> [ { x = x; y = y - 1; dir = 'U' } ]
            | _ -> [ { x = x; y = y + 1; dir = 'D' } ]
        else if current = '-' then
            match dir with
            | 'U' -> [ { x = x - 1; y = y; dir = 'L' }; { x = x + 1; y = y; dir = 'R' } ]
            | 'D' -> [ { x = x - 1; y = y; dir = 'L' }; { x = x + 1; y = y; dir = 'R' } ]
            | 'L' -> [ { x = x - 1; y = y; dir = 'L' } ]
            | _ -> [ { x = x + 1; y = y; dir = 'R' } ]
        else if current = '|' then
            match dir with
            | 'U' -> [ { x = x; y = y - 1; dir = 'U' } ]
            | 'D' -> [ { x = x; y = y + 1; dir = 'D' } ]
            | 'L' -> [ { x = x; y = y - 1; dir = 'U' }; { x = x; y = y + 1; dir = 'D' } ]
            | _ -> [ { x = x; y = y - 1; dir = 'U' }; { x = x; y = y + 1; dir = 'D' } ]
        else
            raise (Exception "Unexpected token")

let getVisits mirrors entry =
    let rec run visited (toVisit: Visit List) =
        if toVisit.Length = 0 then
            visited
        else
            let visit = toVisit.[toVisit.Length - 1]
            let newToVisit = toVisit |> List.rev |> List.tail |> List.rev

            if not (inRange visit.x visit.y mirrors) then
                run visited newToVisit
            else if visited |> List.exists (fun v -> v = visit) then
                run visited newToVisit
            else
                run (visited @ [ visit ]) (newToVisit @ getToVisit mirrors visit)

    run [] [ entry ]

let getEnergized visited =
    visited |> List.map (fun v -> (v.x, v.y)) |> List.distinct |> List.length

let getMirrors (line: string List) =
    line |> List.map (fun s -> s.ToCharArray() |> Array.toList)

let possibleEntryVisits (mirrors: Mirrors) =
    let maxX = mirrors.[0].Length - 1
    let maxY = mirrors.Length - 1

    let top = [ 0..maxX ] |> List.map (fun x -> { x = x; y = 0; dir = 'D' })
    let bottom = [ 0..maxX ] |> List.map (fun x -> { x = x; y = maxY; dir = 'U' })
    let left = [ 0..maxY ] |> List.map (fun y -> { x = 0; y = y; dir = 'R' })
    let right = [ 0..maxY ] |> List.map (fun y -> { x = maxX; y = y; dir = 'L' })

    top @ bottom @ left @ right

let part1 input =
    let mirrors = getMirrors input
    getVisits mirrors { x = 0; y = 0; dir = 'R' } |> getEnergized |> string

let part2 input =
    let mirrors = getMirrors input
    let entries = possibleEntryVisits mirrors
    let visits = entries |> List.map (fun e -> getVisits mirrors e)
    visits |> List.map getEnergized |> List.max |> string
