module Day14

open System

type Map = char List List

let getMap (input: string List) : Map =
    input |> List.map (fun s -> s.ToCharArray() |> Array.toList)

let canRoll (map: Map) (x: int) (y: int) : bool = map.[y].[x] = 'O'

let canMoveDown (map: Map) (x: int) (y: int) : int =
    if y = 0 then
        0
    else if not (canRoll map x y) then
        0
    else
        let rec run (index: int) (total: int) =
            if index < 0 then
                total
            else if map.[index].[x] = '.' then
                run (index - 1) (total + 1)
            else
                total

        run (y - 1) 0

let moveDown (map: Map) (x: int) (y: int) (steps: int) : Map =
    let rec run (index: int) (map: Map) =
        if index = map.Length then
            map
        else if

            index = y
        then
            let currentRow = map.[index]
            let previousRow = map.[index - steps]
            let newCurrentRow = currentRow |> List.mapi (fun i c -> if i = x then '.' else c)
            let newPreviousRow = previousRow |> List.mapi (fun i c -> if i = x then 'O' else c)

            let newMap =
                map
                |> List.mapi (fun i row ->
                    if i = index then newCurrentRow
                    else if i = index - steps then newPreviousRow
                    else row)

            run (index + 1) newMap
        else
            run (index + 1) map

    run 0 map

let moveEverythingDown (map: Map) : Map =
    let rec runRows (y: int) (newMap: Map) =
        if y = map.Length then
            newMap
        else
            let rec runItem (x: int) (newNewMap: Map) =
                if x = newNewMap.[0].Length then
                    newNewMap
                else
                    let canMove = canMoveDown newNewMap x y

                    if canMove = 0 then
                        runItem (x + 1) newNewMap
                    else
                        let newNewNewMap = moveDown newNewMap x y canMove
                        runItem (x + 1) newNewNewMap

            runRows (y + 1) (runItem 0 newMap)

    runRows 0 map

let getTotalLoad (map: Map) : int =
    let rec run (y: int) (total: int) =
        if y = map.Length then
            total
        else
            let scale = map.Length - y

            let rec run2 (x: int) (total: int) =
                if x = map.[0].Length then
                    total
                else
                    let value = if map.[y].[x] = 'O' then 1 else 0
                    run2 (x + 1) (total + value * scale)

            run (y + 1) (total + run2 0 0)

    run 0 0

let part1 (input: string List) =
    let map = getMap input
    let down = moveEverythingDown map
    let totalLoad = getTotalLoad down
    totalLoad |> string

let part2 (input: string List) = "meow"
