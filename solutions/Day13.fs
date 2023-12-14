module Day13

open System

type Pattern = bool List List

let getPattern (input: string List) : Pattern =
    input
    |> List.map (fun s -> s.ToCharArray() |> Array.toList |> List.map (fun c -> c = '#'))

let getPatterns (input: string List) : Pattern List =
    let rec run (index: int) (patterns: Pattern List) (lastSpace: int) : Pattern List =
        let pattern: string List = input.[lastSpace .. index - 1]

        if index = input.Length then
            patterns @ [ getPattern pattern ]
        else if input[index] = "" then
            run (index + 1) (patterns @ [ getPattern pattern ]) (index + 1)
        else
            run (index + 1) patterns lastSpace

    run 0 [] 0

let isVerticalMatch (first: bool List) (second: bool List) : bool =
    let rec run (index: int) : bool =
        if index = first.Length then true
        else if first.[index] = second.[index] then run (index + 1)
        else false

    run 0

let getVerticalIsMirror (pattern: Pattern) (pos: int) : bool =
    let rec run (index: int) : bool =
        if index + pos > pattern.Length || pos - index < 0 then
            true
        else if isVerticalMatch pattern[pos - index] pattern[pos + index - 1] then
            run (index + 1)
        else
            false

    run 0

let findVerticalMirror (pattern: Pattern) : int =
    let rec run (index: int) : int =
        if index = pattern.Length then -1
        else if getVerticalIsMirror pattern index then index
        else run (index + 1)

    run 1

let isHorizontalMatch (pattern: Pattern) (pos: int) (distance: int) : bool =
    let rec run (index: int) : bool =
        if index = pattern.Length then
            true
        else if pattern.[index].[pos - distance] = pattern.[index].[pos + distance - 1] then
            run (index + 1)
        else
            false

    run 0

let getHorizontalIsMirror (pattern: Pattern) (pos: int) : bool =
    let rec run (index: int) : bool =
        if index + pos > pattern[0].Length || pos - index < 0 then
            true
        else if isHorizontalMatch pattern pos index then
            run (index + 1)
        else
            false

    run 1

let findHorizontalMirror (pattern: Pattern) : int =
    let rec run (index: int) : int =
        if index = pattern[0].Length then -1
        else if getHorizontalIsMirror pattern index then index
        else run (index + 1)

    run 1

let points (pattern: Pattern) : int =
    let verticalMirror = findVerticalMirror pattern
    let horizontalMirror = findHorizontalMirror pattern

    if verticalMirror > -1 then verticalMirror * 100
    else if horizontalMirror > -1 then horizontalMirror
    else raise (Exception("No mirror found"))

let part1 (input: string List) =
    let patterns = getPatterns input
    patterns |> List.map points |> List.sum |> string

let part2 (input: string List) = "meow"
