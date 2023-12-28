module Day1

open System

let numbers =
    [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]

let toInt c = Int32.Parse(c.ToString())
let combine (a: int) (b: int) = a * 10 + b

let isNumber (s: string) =
    numbers |> List.exists (fun num -> s.EndsWith(num))

let toNumber (s: string) =
    (numbers |> List.findIndex (fun num -> s.EndsWith(num))) + 1

let getSum (input: string List) (includeText: bool) =
    let rec run (sum: int) (first: int) (last: int) (text: string) (line: string) (index: int) =
        if index = line.Length then
            sum + combine first last
        else
            let c = line.[index]

            if Char.IsDigit c then
                if first = -1 then
                    run sum (toInt c) (toInt c) "" line (index + 1)
                else
                    run sum first (toInt c) "" line (index + 1)
            else if includeText then
                let newText = text + c.ToString()

                if isNumber newText then
                    let newLast = toNumber newText

                    if first = -1 then
                        run sum (toNumber newText) newLast newText line (index + 1)
                    else
                        run sum first newLast newText line (index + 1)
                else
                    run sum first last newText line (index + 1)
            else
                run sum first last text line (index + 1)

    input |> List.fold (fun acc line -> run acc (-1) 0 "" line 0) 0 |> string

let part1 (input: string List) = getSum input false

let part2 (input: string List) = getSum input true
