module Day7

open System

let CARDS_PART_ONE =
    [ '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'J'; 'Q'; 'K'; 'A' ]

let CARDS_PART_TWO =
    [ 'J'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'Q'; 'K'; 'A' ]

type Hand = { cards: int List; bet: int }

type Result = { bet: int; score: float }

let hand (cards: char List) (input: string) : Hand =
    let parts = input.Split(" ")

    { cards =
        parts.[0].ToCharArray()
        |> List.ofArray
        |> List.map (fun s -> List.findIndex (fun c -> c = s) cards)
      bet = Int32.Parse(parts.[1]) }

let hands (input: string list) (cards: char List) : Hand List = input |> List.map (hand cards)

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

let handScore (pretend: bool, hand: Hand) : float =
    let rec run (hand: Hand) : float =
        if not pretend then
            handTypePoints hand
        else if List.exists (fun card -> card = 0) hand.cards then
            let index = List.findIndex (fun card -> card = 0) hand.cards

            let options: Hand List =
                [ 1..13 ]
                |> List.map (fun card ->
                    { cards = hand.cards.[0 .. index - 1] @ [ card ] @ hand.cards.[index + 1 .. 4]
                      bet = hand.bet })

            options |> List.map (fun hand -> run hand) |> List.max

        else
            handTypePoints hand

    let handTypePoints = run hand
    handTypePoints * 10.0 ** 14.0 + tieBreaker hand.cards

let result (pretend: bool, hand: Hand) : Result =
    { bet = hand.bet
      score = handScore (pretend, hand) }

let solution (cards: char List) (input: string List) (pretend: bool) =
    let hands = hands input cards
    let results = hands |> List.map (fun hand -> result (pretend, hand))

    results
    |> List.sortBy (fun result -> result.score)
    |> List.mapi (fun i result -> result.bet * (i + 1))
    |> List.sum
    |> string

let part1 (input: string List) = solution CARDS_PART_ONE input false

let part2 (input: string List) = solution CARDS_PART_TWO input true
