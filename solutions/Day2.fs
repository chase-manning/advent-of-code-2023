module Day2

open System

type Round = { red: int; green: int; blue: int }
type Game = { Id: int; Rounds: Round list }

let get_id (line: string) =
    let id = line.Split(' ')[1]
    Int32.Parse(id.[.. (id.Length - 2)])

let is_color (color: string) (pick: string) = pick.EndsWith(color)

let color_amount (pick: string) = Int32.Parse(pick.Split(' ')[0])

let get_count picks color =
    picks
    |> List.filter (is_color color)
    |> List.fold (fun acc pick -> acc + color_amount pick) 0

let get_round (text: string) =
    let picks = text.Split(", ") |> Array.toList

    { red = get_count picks "red"
      green = get_count picks "green"
      blue = get_count picks "blue" }


let get_rounds (line: string) =
    (line.Split(": ")[1]).Split("; ") |> Array.toList |> List.map get_round

let get_game line =
    { Id = get_id line
      Rounds = get_rounds line }

let get_games lines = List.map get_game lines

let is_round_possible round =
    round.red <= 12 && round.green <= 13 && round.blue <= 14

let is_game_possible game =
    List.forall is_round_possible game.Rounds

let part1 input =
    get_games input
    |> List.filter is_game_possible
    |> List.fold (fun acc game -> acc + game.Id) 0
    |> string

let max_colors game =
    game.Rounds
    |> List.fold
        (fun acc round ->
            { red = Math.Max(acc.red, round.red)
              green = Math.Max(acc.green, round.green)
              blue = Math.Max(acc.blue, round.blue) })
        { red = 0; green = 0; blue = 0 }

let power (round: Round) : int = round.red * round.green * round.blue

let part2 (input: string List) =
    get_games input
    |> List.fold (fun acc game -> (max_colors game |> power) + acc) 0
    |> string
