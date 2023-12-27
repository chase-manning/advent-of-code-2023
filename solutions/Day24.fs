module Day24

open System

type Coord = { x: float; y: float }

type Hailstone = { point: Coord; velocity: Coord }

let getHailstone (input: string) : Hailstone =
    let parts =
        input.Split([| '@'; ','; ':'; 'x'; ' ' |], StringSplitOptions.RemoveEmptyEntries)

    let point =
        { x = float (parts.[0])
          y = float (parts.[1]) }

    let velocity =
        { x = float (parts.[3])
          y = float (parts.[4]) }

    { point = point; velocity = velocity }

let getHailstones (input: string List) : Hailstone List = input |> List.map getHailstone

// let getIntersection (a: Hailstone) (b: Hailstone) : Coord =
//     // Solve the formula to find the intersection point of two lines
//     // a_z = a.velocity.x * t + a.point.x + a.velocity.y * t + a.point.y
//     // b_z = b.velocity.x * t + b.point.x + b.velocity.y * t + b.point.y
//     // a.velocity.x * t + a.point.x + a.velocity.y * t + a.point.y = b.velocity.x * t + b.point.x + b.velocity.y * t + b.point.y
//     // a.velocity.x * t + a.velocity.y * t = b.velocity.x * t + b.velocity.y * t + b.point.x + b.point.y - a.point.x - a.point.y
//     // t * (a.velocity.x + a.velocity.y - b.velocity.x - b.velocity.y) = b.point.x + b.point.y - a.point.x - a.point.y
//     // t = (b.point.x + b.point.y - a.point.x - a.point.y) / (a.velocity.x + a.velocity.y - b.velocity.x - b.velocity.y)
//     printfn "%A" (b.point.x + b.point.y - a.point.x - a.point.y)
//     printfn "%A" (a.velocity.x + a.velocity.y - b.velocity.x - b.velocity.y)

//     let t =
//         (b.point.x + b.point.y - a.point.x - a.point.y)
//         / (a.velocity.x + a.velocity.y - b.velocity.x - b.velocity.y)

//     printfn "%s" "t"
//     printfn "%A" t

//     { x = a.velocity.x * t + a.point.x
//       y = a.velocity.y * t + a.point.y }




let getIntersection (a: Hailstone) (b: Hailstone) : Coord =
    // Solve the formula to find the intersection point of two lines
    // a_y = a.velocity.y * t + a.point.y
    // b_y = b.velocity.y * t + b.point.y
    // a.velocity.y * t + a.point.y = b.velocity.y * t + b.point.y
    // a.velocity.y * t - b.velocity.y * t = b.point.y - a.point.y
    // t * (a.velocity.y - b.velocity.y) = b.point.y - a.point.y
    // t = (b.point.y - a.point.y) / (a.velocity.y - b.velocity.y)

    // a_x = a.velocity.x * t + a.point.x
    // b_x = b.velocity.x * t + b.point.x
    // a.velocity.x * t + a.point.x = b.velocity.x * t + b.point.x
    // a.velocity.x * t - b.velocity.x * t = b.point.x - a.point.x
    // t * (a.velocity.x - b.velocity.x) = b.point.x - a.point.x
    // t = (b.point.x - a.point.x) / (a.velocity.x - b.velocity.x)

    let t_0 = (b.point.y - a.point.y) / (a.velocity.y - b.velocity.y)
    printfn "%s" "t_0"
    printfn "%A" t_0
    let t_1 = (b.point.x - a.point.x) / (a.velocity.x - b.velocity.x)
    printfn "%s" "t_1"
    printfn "%A" t_1
    let t = 2.33333333

    let a_coords =
        { x = a.velocity.x * t + a.point.x
          y = a.velocity.y * t + a.point.y }

    printfn "%s" "a_coords"
    printfn "%A" a_coords

    let b_coords =
        { x = b.velocity.x * t + b.point.x
          y = b.velocity.y * t + b.point.y }

    printfn "%s" "b_coords"
    printfn "%A" b_coords
    a_coords







let intersectsWithinRange (a: Hailstone) (b: Hailstone) (min: float) (max: float) : bool =
    let intersection = getIntersection a b
    let x = intersection.x
    let y = intersection.y
    x >= min && x <= max && y >= min && y <= max

let getPairs (input: Hailstone List) : (Hailstone * Hailstone) List =
    let rec run (pairs: (Hailstone * Hailstone) List) (index: int) =
        if index = input.Length then
            pairs
        else
            let a = input.[index]

            let rec run2 (pairs: (Hailstone * Hailstone) List) (index2: int) =
                if index2 = input.Length then
                    pairs
                else
                    let b = input.[index2]
                    let pairs = if a <> b then (a, b) :: pairs else pairs

                    run2 pairs (index2 + 1)

            run (run2 pairs (index + 1)) (index + 1)

    run [] 0

let part1 (input: string List) =
    let hailstones = getHailstones input
    let pairs = getPairs hailstones
    let min: float = float ("200000000000000")
    let max: float = float ("400000000000000")

    let meow = getIntersection hailstones.[0] hailstones.[1]
    printfn "%A" meow

    // let intersects =
    //     pairs |> List.filter (fun (a, b) -> intersectsWithinRange a b min max)

    // intersects |> List.length |> string
    "meow"

let part2 (input: string List) = "meow"
