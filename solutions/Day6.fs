module Day6

open System

type Race = { time: Int64; record: Int64 }

// I'm too lazy to parse the input ha ha
let PART_1_RACES: Race List =
    [ { time = 55; record = 401 }
      { time = 99; record = 1485 }
      { time = 97; record = 2274 }
      { time = 93; record = 1405 } ]

let PART_2_RACES: Race List =
    [ { time = 55999793
        record = Int64.Parse("401148522741405") } ]

type Option = { hold: Int64; distance: Int64 }

let options (raceTime: Int64) =
    let rec run (options: Option List) (hold: Int64) : Option List =
        if hold = raceTime then
            options
        else
            let newOption =
                { hold = hold
                  distance = (raceTime - hold) * hold }

            run (newOption :: options) (hold + Int64.Parse("1"))

    run [] 0

let winningOptions (options: Option List) (record: Int64) =
    options |> List.filter (fun option -> option.distance > record) |> List.length

let total (races: Race List) =
    races
    |> List.map (fun race -> winningOptions (options race.time) race.record)
    |> List.fold (fun acc x -> acc * x) 1
    |> string

let part1 (input: string List) = total PART_1_RACES

let part2 (input: string List) = total PART_2_RACES
