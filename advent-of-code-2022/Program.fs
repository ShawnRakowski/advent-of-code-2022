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
