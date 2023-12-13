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
        | "8" ->
            match part with
            | "1" -> Day8.part1 input
            | "2" -> Day8.part2 input
            | _ -> "Unknown part"
        | "9" ->
            match part with
            | "1" -> Day9.part1 input
            | "2" -> Day9.part2 input
            | _ -> "Unknown part"
        | "10" ->
            match part with
            | "1" -> Day10.part1 input
            | "2" -> Day10.part2 input
            | _ -> "Unknown part"
        | "11" ->
            match part with
            | "1" -> Day11.part1 input
            | "2" -> Day11.part2 input
            | _ -> "Unknown part"
        | "12" ->
            match part with
            | "1" -> Day12.part1 input
            | "2" -> Day12.part2 input
            | _ -> "Unknown part"
        | "13" ->
            match part with
            | "1" -> Day13.part1 input
            | "2" -> Day13.part2 input
            | _ -> "Unknown part"
        | "14" ->
            match part with
            | "1" -> Day14.part1 input
            | "2" -> Day14.part2 input
            | _ -> "Unknown part"
        | "15" ->
            match part with
            | "1" -> Day15.part1 input
            | "2" -> Day15.part2 input
            | _ -> "Unknown part"
        | "16" ->
            match part with
            | "1" -> Day16.part1 input
            | "2" -> Day16.part2 input
            | _ -> "Unknown part"
        | "17" ->
            match part with
            | "1" -> Day17.part1 input
            | "2" -> Day17.part2 input
            | _ -> "Unknown part"
        | "18" ->
            match part with
            | "1" -> Day18.part1 input
            | "2" -> Day18.part2 input
            | _ -> "Unknown part"
        | "19" ->
            match part with
            | "1" -> Day19.part1 input
            | "2" -> Day19.part2 input
            | _ -> "Unknown part"
        | "20" ->
            match part with
            | "1" -> Day20.part1 input
            | "2" -> Day20.part2 input
            | _ -> "Unknown part"
        | "21" ->
            match part with
            | "1" -> Day21.part1 input
            | "2" -> Day21.part2 input
            | _ -> "Unknown part"
        | "22" ->
            match part with
            | "1" -> Day22.part1 input
            | "2" -> Day22.part2 input
            | _ -> "Unknown part"
        | "23" ->
            match part with
            | "1" -> Day23.part1 input
            | "2" -> Day23.part2 input
            | _ -> "Unknown part"
        | "24" ->
            match part with
            | "1" -> Day24.part1 input
            | "2" -> Day24.part2 input
            | _ -> "Unknown part"
        | "25" ->
            match part with
            | "1" -> Day25.part1 input
            | "2" -> Day25.part2 input
            | _ -> "Unknown part"
        | _ -> "Unknown day"

    // Print the result
    printfn "%s" result
    0
