module Day25

open System

let getConnections (input: string List) : Map<string, string List> =
    let rec run (connections: Map<string, string List>) (index: int) : Map<string, string List> =
        if index = input.Length then
            connections
        else
            let line = input.[index]
            let parts = line.Split(": ")
            let input = parts.[0]
            let outputs = parts.[1].Split(" ") |> Array.toList

            run (Map.add input outputs connections) (index + 1)

    run Map.empty 0

let expandConnections (connections: Map<string, string List>) : Map<string, string List> =
    let keys: string List = connections.Keys |> List.ofSeq

    let rec run (newConnections: Map<string, string List>) (index: int) : Map<string, string List> =
        if index = keys.Length then
            newConnections
        else
            let key = keys.[index]
            let outputs = connections.[key]

            let rec run2 (newNewConnections: Map<string, string List>) (index2: int) : Map<string, string List> =
                if index2 = outputs.Length then
                    newNewConnections
                else
                    let output = outputs.[index2]
                    let existingConnections = newNewConnections.[output]
                    let meow = Map.add output (key :: existingConnections) newNewConnections

                    run2 meow (index2 + 1)

            run (run2 connections 0) (index + 1)

    run connections 0

let part1 (input: string List) =
    let connections = getConnections input
    printfn "%A" connections
    let connections2 = expandConnections connections
    printfn "%A" connections2
    "meow"

let part2 (input: string List) = "meow"
