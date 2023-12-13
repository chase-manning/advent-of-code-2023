module Day9

let getReport (input: string) =
    input.Split(" ") |> Array.toList |> List.map int

let getReports input = input |> List.map getReport

let getDifference (reports: int List) =
    [ 1 .. reports.Length - 1 ] |> List.map (fun i -> reports.[i] - reports.[i - 1])

let getPrediction reports =
    let rec run difference lastNumbers =
        if difference |> List.forall (fun i -> i = 0) then
            lastNumbers
        else
            run (getDifference difference) (lastNumbers @ [ difference[difference.Length - 1] ])

    run reports [] |> List.sum

let getStartPrediction reports =
    let rec run difference firstNumbers =
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



let part1 input =
    getReports input |> List.map getPrediction |> List.sum |> string

let part2 input =
    getReports input |> List.map getStartPrediction |> List.sum |> string
