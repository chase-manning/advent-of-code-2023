module Day23

type Cell =
    | Wall
    | Path
    | Right
    | Down
    | Left
    | Up

type ForrestMap = Cell List List

type Pos = { x: int; y: int }

type Connection = { node: Pos; distance: int }

let charToCell (slippery: bool) (char: char) : Cell =
    if slippery then
        match char with
        | '#' -> Wall
        | '.' -> Path
        | '>' -> Right
        | '<' -> Left
        | '^' -> Up
        | 'v' -> Down
        | _ -> failwith "Unknown char"
    else
        match char with
        | '#' -> Wall
        | '.' -> Path
        | '>' -> Path
        | '<' -> Path
        | '^' -> Path
        | 'v' -> Path
        | _ -> failwith "Unknown char"

let getMapRow (input: string) (slippery: bool) : Cell List =
    input.ToCharArray() |> List.ofArray |> List.map (charToCell slippery)

let getMap (input: string List) (slippery: bool) : ForrestMap =
    let rec run (index: int) (map: ForrestMap) =
        if index = input.Length then
            map
        else
            let newMapRow = getMapRow input[index] slippery
            run (index + 1) (map @ [ newMapRow ])

    run 0 []

let isPath (map: ForrestMap) (pos: Pos) =
    if pos.x < 0 || pos.x >= map[0].Length then
        false
    else if pos.y < 0 || pos.y >= map.Length then
        false
    else
        let cell = map[pos.y].[pos.x]
        cell = Path

let isNode (map: ForrestMap) (pos: Pos) =
    let cell = map[pos.y].[pos.x]

    if not (cell = Path) then
        false
    else if pos.y = 0 || pos.y = map.Length - 1 then
        true
    else
        let up = isPath map { x = pos.x; y = pos.y - 1 }
        let down = isPath map { x = pos.x; y = pos.y + 1 }
        let left = isPath map { x = pos.x - 1; y = pos.y }
        let right = isPath map { x = pos.x + 1; y = pos.y }
        let total = [ up; down; left; right ] |> List.filter (fun x -> x) |> List.length
        total >= 3


let getNodes (map: ForrestMap) : Pos Set =
    let rec run (nodes: Pos Set) (y: int) (x: int) =
        if y = map.Length then
            nodes
        else if x = map[y].Length then
            run nodes (y + 1) 0
        else if isNode map { x = x; y = y } then
            run (nodes.Add({ x = x; y = y })) y (x + 1)
        else
            run nodes y (x + 1)

    run Set.empty 0 0

let getConnectedNodes (map: ForrestMap) (nodes: Pos Set) (node: Pos) : List<Connection> =
    let rec run (path: Pos Set) (pos: Pos) : List<Connection> =
        if pos.x < 0 || pos.x >= map[0].Length then
            []
        else if pos.y < 0 || pos.y >= map.Length then
            []
        else
            let cell = map[pos.y].[pos.x]

            if cell = Wall then
                []
            else if path.Contains(pos) then
                []
            else
                let newPath = path.Add(pos)

                if nodes.Contains(pos) && not path.IsEmpty then
                    [ { node = pos
                        distance = (newPath |> Set.count) - 1 } ]
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

    run Set.empty node

let getConnections (map: ForrestMap) (nodes: Pos Set) : Map<Pos, Connection List> =
    nodes
    |> Set.fold
        (fun acc node ->
            let connections = getConnectedNodes map nodes node
            Map.add node connections acc)
        Map.empty

let getLongestPath (startNode: Pos) (endNode: Pos) (connections: Map<Pos, Connection List>) : int =
    let rec run (visited: Pos Set) (node: Pos) (distance: int) : int List =
        if node = endNode then
            [ distance ]
        else
            connections.[node]
            |> List.filter (fun c -> not (visited.Contains(c.node)))
            |> List.map (fun c -> run (visited.Add(c.node)) c.node (distance + c.distance))
            |> List.concat

    run Set.empty startNode 0 |> List.max

let part1 (input: string List) =
    let map = getMap input true
    let nodes = getNodes map
    let connections = getConnections map nodes
    let startNode = nodes |> Set.toList |> List.head
    let endNode = nodes |> Set.toList |> List.last
    getLongestPath startNode endNode connections |> string

let part2 (input: string List) =
    let map = getMap input false
    let nodes = getNodes map
    let connections = getConnections map nodes
    let startNode = nodes |> Set.toList |> List.head
    let endNode = nodes |> Set.toList |> List.last
    getLongestPath startNode endNode connections |> string
