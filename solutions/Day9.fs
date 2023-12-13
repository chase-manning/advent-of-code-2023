module Day9

open System

let getReport (input: string) : int List =
    input.Split(" ") |> Array.toList |> List.map int

let getReports (input: string List) : (int List) List = input |> List.map getReport

let getDifference (reports: int List) : int List =
    [ 1 .. reports.Length - 1 ] |> List.map (fun i -> reports.[i] - reports.[i - 1])

let getPrediction (reports: int List) : int =
    let rec run (difference: int List) (lastNumbers: int List) =
        if difference |> List.forall (fun i -> i = 0) then
            lastNumbers
        else
            run (getDifference difference) (lastNumbers @ [ difference[difference.Length - 1] ])

    let lastNumbers = run reports []
    lastNumbers |> List.sum

let getStartPrediction (reports: int List) : int =
    let rec run (difference: int List) (firstNumbers: int List) =
        if difference |> List.forall (fun i -> i = 0) then
            firstNumbers
        else
            run (getDifference difference) (firstNumbers @ [ difference[0] ])

    let lastNumbers = run reports []

    let rec runReduce (index: int) (total: int) =
        if index = -1 then
            total
        else
            runReduce (index - 1) (lastNumbers[index] - total)

    runReduce (lastNumbers.Length - 1) 0



let part1 (input: string List) =
    let reports = getReports input
    let predictions = reports |> List.map getPrediction
    predictions |> List.sum |> string

let part2 (input: string List) =
    let reports = getReports input
    let predictions = reports |> List.map getStartPrediction
    predictions |> List.sum |> string
