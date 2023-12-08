open System.IO

[<EntryPoint>]
let main args =
    // Get the day and part from the command line arguments
    let day = args.[0]
    let part = args.[1]

    // Read the input file
    let readLines (path: string) = File.ReadAllLines(path)
    let input = readLines $"./inputs/day-{day}.txt" |> Array.toList

    // Run the solution
    let result =
        match day with
        | "1" ->
            match part with
            | "1" -> Day1.part1 input
            | "2" -> Day1.part2 input
            | _ -> "Unknown part"
        | "2" ->
            match part with
            | "1" -> Day2.part1 input
            | "2" -> Day2.part2 input
            | _ -> "Unknown part"
        | "3" ->
            match part with
            | "1" -> Day3.part1 input
            | "2" -> Day3.part2 input
            | _ -> "Unknown part"
        | "4" ->
            match part with
            | "1" -> Day4.part1 input
            | "2" -> Day4.part2 input
            | _ -> "Unknown part"
        | "5" ->
            match part with
            | "1" -> Day5.part1 input
            | "2" -> Day5.part2 input
            | _ -> "Unknown part"
        | "6" ->
            match part with
            | "1" -> Day6.part1 input
            | "2" -> Day6.part2 input
            | _ -> "Unknown part"
        | "7" ->
            match part with
            | "1" -> Day7.part1 input
            | "2" -> Day7.part2 input
            | _ -> "Unknown part"
        | _ -> "Unknown day"

    // Print the result
    printfn "%s" result
    0
