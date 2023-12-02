module Day2

let RED = 12
let GREEN = 13
let BLUE = 14

type Round = { red: int; green: int; blue: int }

type Game = { Id: int; Rounds: Round list }

let get_id (line: string) : int =
    let id = line.Split(' ')[1]
    System.Int32.Parse(id.[.. (id.Length - 2)])

let is_color (color: string) (pick: string) : bool = pick.EndsWith(color)

let color_amount (pick: string) : int = System.Int32.Parse(pick.Split(' ')[0])

let get_count picks color : int =
    picks
    |> List.filter (is_color color)
    |> List.fold (fun acc pick -> acc + color_amount pick) 0

let get_round (text: string) : Round =
    let picks = text.Split(", ") |> Array.toList

    { red = get_count picks "red"
      green = get_count picks "green"
      blue = get_count picks "blue" }


let get_rounds (line: string) : Round List =
    (line.Split(": ")[1]).Split("; ") |> Array.toList |> List.map get_round

let get_game line : Game =
    { Id = get_id line
      Rounds = get_rounds line }

let get_games lines : Game List = List.map get_game lines

let is_round_possible round : bool =
    round.red <= RED && round.green <= GREEN && round.blue <= BLUE

let is_game_possible game : bool =
    List.forall is_round_possible game.Rounds


let solvePartOne (input: string List) : string =
    (get_games input)
    |> List.filter is_game_possible
    |> List.fold (fun acc game -> acc + game.Id) 0
    |> string

let solvePartTwo (input: string List) = "TODO"
