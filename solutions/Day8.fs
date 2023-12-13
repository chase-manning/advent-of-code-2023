module Day8

open System

let instructions (input: string List) : char List = input[0].ToCharArray() |> Array.toList

let instruction (instructions: char List) (index: int) : char =
    instructions.[index % instructions.Length]

let path_to_int (path: string) (paths: string List) : int =
    let index = List.findIndex (fun s -> s = path) paths
    index

let node (input: string) (paths: string List) : int * int =
    let left = input.[input.Length - 9 .. input.Length - 7]
    let right = input.[input.Length - 4 .. input.Length - 2]
    (path_to_int left paths, path_to_int right paths)

let nodes (input: string List) : (int * int) List =
    let start_pos = 2

    let items = input |> List.skip start_pos
    // Order alphabetically
    let sorted = items |> List.sortBy (fun s -> s)
    let paths = sorted |> List.map (fun s -> s.Split(" ")[0])
    let nodes = sorted |> List.map (fun s -> node s paths)
    nodes

let part1 (input: string List) =
    let instructions = instructions input
    let nodes = nodes input

    let rec run (position: int) (index: int) =
        if position = nodes.Length - 1 then
            index
        else
            let (left, right) = nodes.[position]
            let instruction = instruction instructions index

            match instruction with
            | 'L' -> run left (index + 1)
            | 'R' -> run right (index + 1)
            | _ -> failwith "Unknown instruction"

    let index = run 0 0
    index.ToString()



let part2 (input: string List) = "meow"
