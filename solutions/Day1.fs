module Day1

let solvePartOne (input: string[]) =
    let isDigit c = System.Char.IsDigit c
    let toInt c = System.Int32.Parse(c.ToString())

    let mutable sum = 0

    for line in input do

        let mutable first = -1
        let mutable last = 0

        for i = 0 to line.Length - 1 do
            let c = line.[i]

            if isDigit c then
                if first = -1 then
                    first <- toInt c

                last <- toInt c

        let number = first * 10 + last
        sum <- sum + number

    sum.ToString()


let solvePartTwo (input: string[]) = "TODO"
