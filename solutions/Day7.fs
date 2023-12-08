module Day7

open System

let CARDS = [ '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'J'; 'Q'; 'K'; 'A' ]

type Hand = { cards: int List; bet: int }

type Result = { bet: int; score: float }

let hand (input: string) : Hand =
    let parts = input.Split(" ")

    { cards =
        parts.[0].ToCharArray()
        |> List.ofArray
        |> List.map (fun s -> List.findIndex (fun c -> c = s) CARDS)
      bet = Int32.Parse(parts.[1]) }

let hands (input: string list) : Hand List = input |> List.map hand

let isFiveOfAKind (hand: Hand) : bool =
    hand.cards |> List.forall (fun card -> card = hand.cards.[0])

let isFourOfAKind (hand: Hand) : bool =
    let cards = hand.cards |> List.sort

    cards.[0] = cards.[1] && cards.[1] = cards.[2] && cards.[2] = cards.[3]
    || cards.[1] = cards.[2] && cards.[2] = cards.[3] && cards.[3] = cards.[4]

let isFullHouse (hand: Hand) : bool =
    let cards = hand.cards |> List.sort

    cards.[0] = cards.[1] && cards.[1] = cards.[2] && cards.[3] = cards.[4]
    || cards.[0] = cards.[1] && cards.[2] = cards.[3] && cards.[3] = cards.[4]

let isThreeOfAKind (hand: Hand) : bool =
    let cards = hand.cards |> List.sort

    cards.[0] = cards.[1] && cards.[1] = cards.[2]
    || cards.[1] = cards.[2] && cards.[2] = cards.[3]
    || cards.[2] = cards.[3] && cards.[3] = cards.[4]

let isTwoPairs (hand: Hand) : bool =
    let cards = hand.cards |> List.sort

    cards.[0] = cards.[1] && cards.[2] = cards.[3]
    || cards.[0] = cards.[1] && cards.[3] = cards.[4]
    || cards.[1] = cards.[2] && cards.[3] = cards.[4]

let isOnePair (hand: Hand) : bool =
    let cards = hand.cards |> List.sort

    cards.[0] = cards.[1]
    || cards.[1] = cards.[2]
    || cards.[2] = cards.[3]
    || cards.[3] = cards.[4]

let handTypePoints (hand: Hand) : float =
    if isFiveOfAKind hand then 6.0
    else if isFourOfAKind hand then 5.0
    else if isFullHouse hand then 4.0
    else if isThreeOfAKind hand then 3.0
    else if isTwoPairs hand then 2.0
    else if isOnePair hand then 1.0
    else 0

let tieBreaker (cards: int List) =
    cards
    |> List.rev
    |> List.mapi (fun i card -> (float card) * (10.0 ** float ((i + 1) * 2)))
    |> List.sum

let handScore (hand: Hand) : float =
    let handTypePoints = handTypePoints hand
    handTypePoints * 10.0 ** 14.0 + tieBreaker hand.cards

let result (hand: Hand) : Result =
    { bet = hand.bet
      score = handScore hand }

let part1 (input: string List) =
    let hands = hands input
    let results = hands |> List.map result

    results
    |> List.sortBy (fun result -> result.score)
    |> List.mapi (fun i result -> result.bet * (i + 1))
    |> List.sum
    |> string

let part2 (input: string List) = "meow"
