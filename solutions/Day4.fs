module Day4

open System

let toNumber s = Int32.Parse(s.ToString())

let numbers (s: string) =
    s.Split(' ') |> Array.toList |> List.map toNumber

let lineParser (line: string) =
    let numberStrings = line.Replace("  ", " ").Split(": ")[1]
    let sides = numberStrings.Split(" | ")
    (numbers sides[0], numbers sides[1])

let points winning selection =
    let correct =
        selection
        |> List.filter (fun x -> winning |> List.exists (fun y -> x = y))
        |> List.length
        |> float

    if correct = 0 then 0 else (2.0 ** (correct - 1.0)) |> int32

let part1 (input: string List) =
    input
    |> List.map lineParser
    |> List.fold (fun acc (winning, selection) -> acc + points winning selection) 0
    |> string

let part2 (input: string List) = "meow"
