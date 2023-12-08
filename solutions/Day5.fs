module Day5

open System

type Rule =
    { source: Int64
      dest: Int64
      len: Int64 }

type Mapping = Rule List

let seeds (lines: string List) =
    let firstLine = lines[0]
    let numbers = firstLine.Split(": ")[1]
    numbers.Split(" ") |> Array.toList |> List.map Int64.Parse

let add_mappings (mapping: Mapping) (line: string) : Mapping =
    let parts = line.Split(" ")
    let dest = Int64.Parse(parts.[0])
    let source = Int64.Parse(parts.[1])
    let len = Int64.Parse(parts.[2])

    { source = source
      dest = dest
      len = len }
    :: mapping



let mapping (lines: string List) (pos: int) : Mapping =
    let rec run (mapping: Mapping) (pos: int) : Mapping =
        if pos = lines.Length then
            mapping
        else if lines.[pos] = "" then
            mapping
        else
            let line = lines.[pos]
            run (add_mappings mapping line) (pos + 1)

    run [] (pos + 1)

let get_mappings (lines: string List) : Mapping List =
    let mutable mappings: Mapping List = []

    for i = 2 to lines.Length - 1 do
        if lines.[i].EndsWith(":") then
            mappings <- mapping lines i :: mappings

    mappings

let output (input: Int64) (mapping: Mapping) : Int64 =
    let rec run pos =
        if pos = mapping.Length then
            input
        else
            let rule = mapping.[pos]

            if input >= rule.source && input < rule.source + rule.len then
                rule.dest + (input - rule.source)
            else
                run (pos + 1)

    run 0


let seedLocation (input: Int64) (mappings: Mapping List) : Int64 =
    let rec run (input: Int64) (pos: int) : Int64 =
        if pos = mappings.Length then
            input
        else
            run (output input mappings.[pos]) (pos + 1)

    run input 0

let part1 (input: string List) =
    let seeds = seeds input
    let mappings = (get_mappings input) |> List.rev
    // printfn "%A" mappings
    let seedLocations = seeds |> List.map (fun seed -> seedLocation seed mappings)
    // printfn "%A" seedLocations
    seedLocations |> List.min |> string

let part2 (input: string List) = "meow"
