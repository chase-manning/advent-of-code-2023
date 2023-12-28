module Day3

open System

type Number =
    { line: int
      start: int
      value: int
      len: int }

let toInt c = System.Int32.Parse(c.ToString())

let rec get_numbers (input: string List) : Number List =
    let rec run (line: int) (pos: int) (start: int) (value: int) (numbers: Number List) =
        let newNumbers =
            ([ { line = line
                 start = start
                 value = value
                 len = pos - start } ]
             @ numbers)

        if line = input.Length then
            numbers
        else if pos = input[line].Length then
            if value = 0 then
                run (line + 1) 0 0 0 numbers
            else
                run (line + 1) 0 0 0 newNumbers
        else if Char.IsNumber(input[line][pos]) then
            (run line (pos + 1) start (value * 10 + toInt (input[line][pos])) numbers)
        else if value = 0 then
            run line (pos + 1) (pos + 1) 0 numbers
        else
            run line (pos + 1) (pos + 1) 0 newNumbers

    run 0 0 0 0 []

let rec has_symbol (input: string List) (line: int) (pos: int) (len: int) : bool =
    if line >= 0 && line < input.Length then
        if pos < 0 then
            has_symbol input line (pos + 1) (len - 1)
        else if pos = input[line].Length then
            false
        else if Char.IsNumber(input[line][pos]) || input[line][pos] = '.' then
            if len <= 1 then
                false
            else
                has_symbol input line (pos + 1) (len - 1)
        else
            true
    else
        false

let rec get_asterisk (input: string List) (line: int) (pos: int) asterisks =
    if line = input.Length then
        asterisks
    else if pos = input[line].Length then
        get_asterisk input (line + 1) 0 asterisks
    else if input[line][pos] = '*' then
        get_asterisk input line (pos + 1) ((line, pos) :: asterisks)
    else
        get_asterisk input line (pos + 1) asterisks


let adjacent_numbers asterisk (numbers: Number List) =
    numbers
    |> List.filter (fun number ->
        number.line <= fst asterisk + 1
        && number.line >= fst asterisk - 1
        && number.start <= snd asterisk + 1
        && (number.start + number.len) >= snd asterisk)

let get_gears (input: string List) =
    (get_asterisk input 0 0 [])
    |> List.filter (fun asterisk -> (adjacent_numbers asterisk (get_numbers input)).Length = 2)

let is_part (input: string List) (number: Number) : bool =
    has_symbol input (number.line - 1) (number.start - 1) (number.len + 2) // Checking above the number
    || has_symbol input (number.line + 1) (number.start - 1) (number.len + 2) // Checking below the number
    || has_symbol input number.line (number.start - 1) 1 // Checking left of the number
    || has_symbol input number.line (number.start + number.len) 1 // Checking right of the number

let part1 (input: string List) =
    (List.filter (is_part input) (get_numbers input))
    |> List.fold (fun acc number -> acc + number.value) 0
    |> string

let gear_ratio gear (numbers: Number List) : int =
    let adjacent = adjacent_numbers gear numbers
    adjacent[0].value * adjacent[1].value

let part2 (input: string List) =
    get_gears input
    |> List.fold (fun acc gear -> acc + (gear_ratio gear (get_numbers input))) 0
    |> string
