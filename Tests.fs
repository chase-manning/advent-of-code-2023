module meow

open System.IO

open NUnit.Framework
open FsUnit

let getInput (day: int) =
    let readLines (path: string) = File.ReadAllLines(path)
    readLines $"../../../inputs/day-{day}.txt" |> Array.toList

[<Test>]
let TestDay1Part1 () =
    getInput 1 |> Day1.part1 |> should equal "54573"

[<Test>]
let TestDay1Part2 () =
    getInput 1 |> Day1.part2 |> should equal "54591"

[<Test>]
let TestDay2Part1 () =
    getInput 2 |> Day2.part1 |> should equal "2439"

[<Test>]
let TestDay2Part2 () =
    getInput 2 |> Day2.part2 |> should equal "63711"

[<Test>]
let TestDay3Part1 () =
    getInput 3 |> Day3.part1 |> should equal "535351"

[<Test>]
let TestDay3Part2 () =
    getInput 3 |> Day3.part2 |> should equal "87287096"

[<Test>]
let TestDay4Part1 () =
    getInput 4 |> Day4.part1 |> should equal "19135"

[<Test>]
let TestDay4Part2 () =
    getInput 4 |> Day4.part2 |> should equal "5704953"

[<Test>]
let TestDay5Part1 () =
    getInput 5 |> Day5.part1 |> should equal "346433842"

// [<Test>]
// let TestDay5Part2 () =
//     getInput 5 |> Day5.part2 |> should equal "55"

[<Test>]
let TestDay6Part1 () =
    getInput 6 |> Day6.part1 |> should equal "2374848"

[<Test>]
let TestDay6Part2 () =
    getInput 6 |> Day6.part2 |> should equal "39132886"

[<Test>]
let TestDay7Part1 () =
    getInput 7 |> Day7.part1 |> should equal "250602641"

[<Test>]
let TestDay7Part2 () =
    getInput 7 |> Day7.part2 |> should equal "251037509"

[<Test>]
let TestDay8Part1 () =
    getInput 8 |> Day8.part1 |> should equal "12361"

// [<Test>]
// let TestDay8Part2 () =
//     getInput 8 |> Day8.part2 |> should equal "2074"

[<Test>]
let TestDay9Part1 () =
    getInput 9 |> Day9.part1 |> should equal "1696140818"

[<Test>]
let TestDay9Part2 () =
    getInput 9 |> Day9.part2 |> should equal "1152"

// [<Test>]
// let TestDay10Part1 () =
//     getInput 10 |> Day10.part1 |> should equal "329356"

// [<Test>]
// let TestDay10Part2 () =
//     getInput 10 |> Day10.part2 |> should equal "4666278"

[<Test>]
let TestDay11Part1 () =
    getInput 11 |> Day11.part1 |> should equal "10077850"

// [<Test>]
// let TestDay11Part2 () =
//     getInput 11 |> Day11.part2 |> should equal "298"

[<Test>]
let TestDay12Part1 () =
    getInput 12 |> Day12.part1 |> should equal "7771"

// [<Test>]
// let TestDay12Part2 () =
//     getInput 12 |> Day12.part2 |> should equal "87842"

[<Test>]
let TestDay13Part1 () =
    getInput 13 |> Day13.part1 |> should equal "33780"

// [<Test>]
// let TestDay13Part2 () =
//     getInput 13 |> Day13.part2 |> should equal "725"

[<Test>]
let TestDay14Part1 () =
    getInput 14 |> Day14.part1 |> should equal "105982"

// [<Test>]
// let TestDay14Part2 () =
//     getInput 14 |> Day14.part2 |> should equal "1084"

[<Test>]
let TestDay15Part1 () =
    getInput 15 |> Day15.part1 |> should equal "514394"

// [<Test>]
// let TestDay15Part2 () =
//     getInput 15 |> Day15.part2 |> should equal "117936"

[<Test>]
let TestDay16Part1 () =
    getInput 16 |> Day16.part1 |> should equal "7496"

[<Test>]
let TestDay16Part2 () =
    getInput 16 |> Day16.part2 |> should equal "7932"

// [<Test>]
// let TestDay17Part1 () =
//     getInput 17 |> Day17.part1 |> should equal "376008"

// [<Test>]
// let TestDay17Part2 () =
//     getInput 17 |> Day17.part2 |> should equal "18"

// [<Test>]
// let TestDay18Part1 () =
//     getInput 18 |> Day18.part1 |> should equal "768"

// [<Test>]
// let TestDay18Part2 () =
//     getInput 18 |> Day18.part2 |> should equal "781"

[<Test>]
let TestDay19Part1 () =
    getInput 19 |> Day19.part1 |> should equal "376008"

[<Test>]
let TestDay19Part2 () =
    getInput 19 |> Day19.part2 |> should equal "124078207789312"

// [<Test>]
// let TestDay20Part1 () =
//     getInput 20 |> Day20.part1 |> should equal "831600"

// [<Test>]
// let TestDay20Part2 () =
//     getInput 20 |> Day20.part2 |> should equal "884520"

// [<Test>]
// let TestDay21Part1 () =
//     getInput 21 |> Day21.part1 |> should equal "111"

// [<Test>]
// let TestDay21Part2 () =
//     getInput 21 |> Day21.part2 |> should equal "188"

// [<Test>]
// let TestDay22Part1 () =
//     getInput 22 |> Day22.part1 |> should equal "953"

// [<Test>]
// let TestDay22Part2 () =
//     getInput 22 |> Day22.part2 |> should equal "1289"

// [<Test>]
// let TestDay23Part1 () =
//     getInput 23 |> Day23.part1 |> should equal "2310"

// [<Test; Timeout(600000)>]
// let TestDay23Part2 () =
//     getInput 23 |> Day23.part2 |> should equal "6738"

// [<Test>]
// let TestDay24Part1 () =
//     getInput 24 |> Day24.part1 |> should equal "10439961859"

// [<Test>]
// let TestDay24Part2 () =
//     getInput 24 |> Day24.part2 |> should equal "72050269"

// [<Test>]
// let TestDay25Part1 () =
//     getInput 25 |> Day25.part1 |> should equal "19980801"

// [<Test>]
// let TestDay25Part2 () =
//     getInput 25 |> Day25.part2 |> should equal "meow"
