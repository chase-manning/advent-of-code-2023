module Day12

open System

let getSprings (input: string) : char List =
    input.Split(" ").[0].ToCharArray() |> List.ofArray

let getGroups (input: string) : int List =
    input.Split(" ").[1].ToString().Split(",") |> List.ofArray |> List.map int

let getSpringsAndGroups (input: string) : (char List * int List) = (getSprings input, getGroups input)

let getSpringOptions (springs: char List) : (char List) List =
    let rec run (index: int) (options: (char List) List) =
        if index = springs.Length then
            options
        else if springs.[index] = '?' then
            let operational = options |> List.map (fun option -> option @ [ '.' ])
            let damaged = options |> List.map (fun option -> option @ [ '#' ])
            let joined = operational @ damaged
            run (index + 1) joined
        else
            run (index + 1) (options |> List.map (fun option -> option @ [ springs.[index] ]))

    run 0 [ [] ]

let getGroupFromSpring (spring: char List) : int List =
    let rec run (index: int) (group: int List) : int List =
        if index = spring.Length then
            group
        else if spring.[index] = '.' then
            if group.[group.Length - 1] = 0 then
                run (index + 1) group
            else
                run (index + 1) (group @ [ 0 ])
        else
            let newGroup =
                group |> List.mapi (fun i g -> if i = group.Length - 1 then g + 1 else g)

            run (index + 1) newGroup

    let output = run 0 [ 0 ]

    if output.[output.Length - 1] = 0 then
        output |> List.rev |> List.tail |> List.rev
    else
        output


let getArrangements (springs: char List) (group: int List) : int =
    let options = getSpringOptions springs
    // printfn "%s" "Options:"
    // printfn "%A" options
    let optionGroups = options |> List.map getGroupFromSpring
    // printfn "%s" "Option Groups:"
    // printfn "%A" optionGroups
    // printfn "%s" "Group:"
    // printfn "%A" group
    let validGroups = optionGroups |> List.filter (fun g -> g = group)
    // printfn "%s" "Valid Groups:"
    // printfn "%A" validGroups
    validGroups.Length

let part1 (input: string List) =
    let springsAndGroups = input |> List.map getSpringsAndGroups
    // let first = springsAndGroups.[0]
    // let arrangements = getArrangements (fst first) (snd first)

    let arrangements =
        springsAndGroups
        |> List.map (fun (springs, group) -> getArrangements springs group)

    arrangements |> List.sum |> string
// "meow"

let part2 (input: string List) = "meow"
