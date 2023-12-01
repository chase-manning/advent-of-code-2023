open System.IO

[<EntryPoint>]
let main args =
    // Get the day and part from the command line arguments
    let day = args.[0]
    let part = args.[1]

    // Read the input file
    let readLines (path: string) = File.ReadAllLines(path)
    let input = readLines $"./inputs/day-{day}.txt"

    // Run the solution
    let result =
        match day with
        | "1" ->
            match part with
            | "1" -> Day1.solvePartOne input
            | "2" -> Day1.solvePartTwo input
            | _ -> "Unknown part"
        | "2" ->
            match part with
            | "1" -> Day2.solvePartOne input
            | "2" -> Day2.solvePartTwo input
            | _ -> "Unknown part"
        | _ -> "Unknown day"

    // Print the result
    printfn "%s" result
    0
