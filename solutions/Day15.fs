module Day15

open System

let hash (input: string) : int =
    let chars = input.ToCharArray() |> Array.toList

    let rec run (index: int) (total: int) =
        if index = chars.Length then
            total
        else
            let char = chars.[index]
            let ascii = Convert.ToInt32 char
            let newTotal = ((total + ascii) * 17) % 256
            run (index + 1) newTotal

    run 0 0

let getInstructions (input: string List) = input[0].Split(",") |> Array.toList

let part1 (input: string List) =
    let instructions = getInstructions input
    instructions |> List.map hash |> List.sum |> string


let part2 (input: string List) = "meow"
