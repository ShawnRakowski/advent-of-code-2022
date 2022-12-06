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

// revised 2
"input3.txt"
    |> File.ReadAllLines
    |> Seq.map(fun r ->
        r[..r.Length/2] |> Seq.find(r[r.Length/2..].Contains) |> int
        |> fun c -> c - if c > 96 then 96 else 38
    )
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

// -------------------------------------------------------

let split c (str:string) = str.Split [|c|]

"input4.txt"
    |> File.ReadAllLines
    |> Seq.map(fun line -> line
                        |> split ','
                        |> Seq.map(split '-')
                        |> Seq.map(fun pair -> set {int pair[0]..int pair[1]})
                        |> Seq.toList
                        |> function
                           | [l1;l2;] -> l1.IsSubsetOf(l2) || l2.IsSubsetOf(l1)
                           | _ -> false
    )
    |> Seq.filter(fun t -> t)
    |> Seq.length
    |> printfn "%d"

"input4.txt"
    |> File.ReadAllLines
    |> Seq.map(fun line -> line
                        |> split ','
                        |> Seq.map(split '-')
                        |> Seq.map(fun pair -> set {int pair[0]..int pair[1]})
                        |> Seq.toList
                        |> function
                           | [l1;l2;] -> Set.intersect l1 l2
                           | _ -> Set.empty
    )
    |> Seq.filter(fun s -> s.Count > 0)
    |> Seq.length
    |> printfn "%d"

// -------------------------------------------------------

let input5 = "input5.txt" |> File.ReadAllLines
let realize x = x |> Seq.toArray |> Array.toSeq

let stacks = input5 
             |> Seq.takeWhile(not << String.IsNullOrWhiteSpace)
             |> Seq.rev
             |> Seq.map(fun line -> line + " "
                                 |> Seq.chunkBySize 4
                                 |> Seq.map(fun c -> c |> String)
                                 |> Seq.map(fun c -> c.Trim [|' ';'[';']'|])
             )
             |> Seq.skip 1
             |> fun rows -> (rows |> Seq.head |> Seq.map(fun _ -> list<string>.Empty), rows)
             ||> Seq.fold(fun acc curr -> Seq.zip acc curr |> Seq.map(fun (a, b) -> a @ [b]))
             |> Seq.map(Seq.filter(not << String.IsNullOrWhiteSpace))
             |> realize

let commands = input5 
               |> Seq.skipWhile(not << String.IsNullOrWhiteSpace)
               |> Seq.skip 1
               |> Seq.filter(not << String.IsNullOrWhiteSpace)
               |> Seq.map(fun cmd -> cmd.Split())
               |> Seq.map(fun cmd -> (int cmd[1], int cmd[3] - 1, int cmd[5] - 1))
               |> realize

(stacks, commands)
    ||> Seq.fold(fun stks (c, si, di) -> (stks |> Seq.item(si) |> Seq.rev, stks |> Seq.item(di))
                                      |> fun (revSrc, dst) -> (revSrc |> Seq.skip(c) |> Seq.rev, Seq.append dst (revSrc |> Seq.take(c)))
                                      |> fun (nSrc, nDst) -> (stks |> Seq.updateAt si nSrc, nDst)
                                      |> fun (nStks, nDst) -> nStks |> Seq.updateAt di nDst
                                      |> Seq.map(realize)
                                      |> realize
    )
    |> Seq.map(Seq.last)
    |> String.concat String.Empty
    |> printfn "%s"

(stacks, commands)
    ||> Seq.fold(fun stks (c, si, di) -> (stks |> Seq.item(si) |> Seq.rev, stks |> Seq.item(di))
                                      |> fun (revSrc, dst) -> (revSrc |> Seq.skip(c) |> Seq.rev, Seq.append dst (revSrc |> Seq.take(c) |> Seq.rev))
                                      |> fun (nSrc, nDst) -> (stks |> Seq.updateAt si nSrc, nDst)
                                      |> fun (nStks, nDst) -> nStks |> Seq.updateAt di nDst
                                      |> Seq.map(realize)
                                      |> realize
    )
    |> Seq.map(Seq.last)
    |> String.concat String.Empty
    |> printfn "%s"