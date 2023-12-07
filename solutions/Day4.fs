module Day4

open System

let toNumber s = Int32.Parse(s.ToString())

let numbers (s: string) =
    s.Split(' ') |> Array.toList |> List.map toNumber

let lineParser (line: string) =
    let numberStrings = line.Replace("  ", " ").Split(": ")[1]
    let sides = numberStrings.Split(" | ")
    (numbers sides[0], numbers sides[1])

let correct (winning: int List) (selection: int List) =
    selection
    |> List.filter (fun x -> winning |> List.exists (fun y -> x = y))
    |> List.length

let points winning selection =
    let correct = correct winning selection |> float
    if correct = 0 then 0 else (2.0 ** (correct - 1.0)) |> int32

let part1 (input: string List) =
    input
    |> List.map lineParser
    |> List.fold (fun acc (winning, selection) -> acc + points winning selection) 0
    |> string

let totalCards (points: int List) : int =
    let rec run (cards: int List) pos : int =
        let rec addCards (cards_: int List) (pos: int) (toAdd: int) (scale: int) : int List =
            if toAdd = 0 then
                cards_
            else if pos = cards_.Length then
                cards_
            else
                let newCards =
                    cards_ |> List.mapi (fun i card -> if i = pos then card + scale else card)

                addCards newCards (pos + 1) (toAdd - 1) scale

        if pos = points.Length then
            cards |> List.sum
        else
            let newCards = addCards cards (pos + 1) points.[pos] cards.[pos]
            run newCards (pos + 1)

    run (points |> List.map (fun _ -> 1)) 0

let part2 (input: string List) =
    let cards = input |> List.map lineParser

    let points =
        cards |> List.map (fun (winning, selection) -> correct winning selection)

    totalCards points |> string
