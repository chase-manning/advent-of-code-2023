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

let sorted_lines (input: string List) : string List =
    let start_pos = 2
    let items = input |> List.skip start_pos
    let sorted = items |> List.sortBy (fun s -> s)
    sorted

let paths (input: string List) : string List =
    let sorted = sorted_lines input
    let paths = sorted |> List.map (fun s -> s.Split(" ")[0])
    paths

let nodes (input: string List) : (int * int) List =
    let sorted = sorted_lines input
    let paths = paths input
    let nodes = sorted |> List.map (fun s -> node s paths)
    nodes

let starts (input: string List) : int List =
    let paths_ = paths input

    paths_
    |> List.filter (fun s -> s.EndsWith("A"))
    |> List.map (fun s -> path_to_int s (paths input))

let ends (input: string List) : int List =
    let paths_ = paths input

    paths_
    |> List.filter (fun s -> s.EndsWith("Z"))
    |> List.map (fun s -> path_to_int s (paths input))

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

let loop_and_ends
    (start: int)
    (nodes: (int * int) List)
    (instructions: char List)
    (ends: int List)
    : (int * (int List)) =
    let rec run (position: int) (index: int) (end_indexes: int List) : (int * (int List)) =
        if position = start && end_indexes.Length > 0 then
            (index, end_indexes)
        else
            let (left, right) = nodes.[position]
            let instruction = instruction instructions index

            run
                (match instruction with
                 | 'L' -> left
                 | 'R' -> right
                 | _ -> failwith "Unknown instruction")
                (index + 1)
                (if List.exists (fun e -> e = position) ends then
                     end_indexes @ [ position ]
                 else
                     end_indexes)

    run start 0 []

let part2 (input: string List) =
    let instructions = instructions input
    let nodes = nodes input
    let starts = starts input
    let ends = ends input

    let starts = starts |> List.map (fun s -> loop_and_ends s nodes instructions ends)
    printfn "%A" starts


    "meow"
