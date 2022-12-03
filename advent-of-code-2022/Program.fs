open System
open System.IO

let input = (([], 0), File.ReadAllLines("input.txt"))
            ||> Seq.fold(fun (l, c) curr -> if String.IsNullOrWhiteSpace(curr) then ((l @ [c]), 0) else (l, c + int curr))
            |> fun (l, c) -> l @ [c]

input
    |> Seq.max
    |> printfn "%d"

input
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.sum
    |> printfn "%d"

"input2.txt"
    |> File.ReadAllLines
    |> Seq.map(function
                | "A X" -> 1 + 3 // ROCK ROCK
                | "A Y" -> 2 + 6 // ROCK PAPER
                | "A Z" -> 3 + 0 // ROCK SCISSORS
                | "B X" -> 1 + 0 // PAPER ROCK
                | "B Y" -> 2 + 3 // PAPER PAPER
                | "B Z" -> 3 + 6 // PAPER SCISSORS
                | "C X" -> 1 + 6 // SCISSORS ROCK
                | "C Y" -> 2 + 0 // SCISSORS PAPER
                | "C Z" -> 3 + 3 // SCISSORS SCISSORS
                | _ -> 0
    ) 
    |> Seq.sum
    |> printfn "%d"

// -------------------------------------------------------

type RpsOpts =
    | Rock = 1
    | Paper = 2
    | Scissors = 3

let loseTo rpsOpt =
    match rpsOpt with
    | RpsOpts.Rock -> RpsOpts.Scissors |> int
    | RpsOpts.Paper -> RpsOpts.Rock |> int
    | RpsOpts.Scissors -> RpsOpts.Paper |> int
    | _ -> 0

let winOver rpsOpt =
    match rpsOpt with
    | RpsOpts.Rock -> RpsOpts.Paper |> int
    | RpsOpts.Paper -> RpsOpts.Scissors |> int
    | RpsOpts.Scissors -> RpsOpts.Rock |> int
    | _ -> 0

"input2.txt"
    |> File.ReadAllLines
    |> Seq.map(function
                | "A X" -> loseTo RpsOpts.Rock + 0      // ROCK LOSE
                | "A Y" -> int RpsOpts.Rock + 3         // ROCK DRAW
                | "A Z" -> winOver RpsOpts.Rock + 6     // ROCK WIN
                | "B X" -> loseTo RpsOpts.Paper + 0     // PAPER LOSE
                | "B Y" -> int RpsOpts.Paper + 3        // PAPER DRAW
                | "B Z" -> winOver RpsOpts.Paper + 6    // PAPER WIN
                | "C X" -> loseTo RpsOpts.Scissors + 0  // SCISSORS LOSE
                | "C Y" -> int RpsOpts.Scissors + 3     // SCISSORS DRAW
                | "C Z" -> winOver RpsOpts.Scissors + 6 // SCISSORS WIN
                | _ -> 0
    ) 
    |> Seq.sum
    |> printfn "%d"

// -------------------------------------------------------

"input3.txt"
    |> File.ReadAllLines
    |> Seq.map(fun s -> (s.Substring(0, s.Length / 2), s.Substring(s.Length / 2, s.Length / 2)))
    |> Seq.map(fun (comp1, comp2) -> comp1 |> Seq.filter(fun c -> comp2.Contains(c)))
    |> Seq.map(fun c -> c |> Seq.head)
    |> Seq.map(fun c -> if c >= 'a' && c <= 'z' then (int((c - 'a') + (char)1)) else (int((c - 'A') + (char)27)))
    |> Seq.sum
    |> printfn "%d"

"input3.txt"
    |> File.ReadAllLines
    |> Seq.chunkBySize 3
    |> Seq.map(fun x -> (x[0], x[1], x[2]))
    |> Seq.map(fun (comp1, comp2, comp3) -> comp1 |> Seq.filter(fun c -> comp2.Contains(c) && comp3.Contains(c)))
    |> Seq.map(fun c -> c |> Seq.head)
    |> Seq.map(fun c -> if c >= 'a' && c <= 'z' then (int((c - 'a') + (char)1)) else (int((c - 'A') + (char)27)))
    |> Seq.sum
    |> printfn "%d"        
