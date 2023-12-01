module Day1

let numbers =
    [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]

let isDigit c = System.Char.IsDigit c
let toInt c = System.Int32.Parse(c.ToString())
let combine (a: int) (b: int) = a * 10 + b

let isNumber (s: string) =
    numbers |> List.exists (fun num -> s.EndsWith(num))

let toNumber (s: string) =
    (numbers |> List.findIndex (fun num -> s.EndsWith(num))) + 1

let getSum (input: string[]) (includeText: bool) =
    let mutable sum = 0

    for line in input do
        let mutable text = ""
        let mutable first = -1
        let mutable last = 0

        for i = 0 to line.Length - 1 do
            let c = line.[i]

            if isDigit c then
                if first = -1 then
                    first <- toInt c

                last <- toInt c
                text <- ""
            else if includeText then
                text <- text + c.ToString()

                if isNumber text then
                    if first = -1 then
                        first <- toNumber text

                    last <- toNumber text

        sum <- sum + combine first last

    sum.ToString()

let solvePartOne (input: string[]) = getSum input false

let solvePartTwo (input: string[]) = getSum input true
