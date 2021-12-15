open System.IO

let result =
  File.ReadLines("input.txt")
  |> Seq.map(fun n -> (int n/3) - 2)
  |> Seq.reduce(fun a b -> a + b)

printfn "%d" result