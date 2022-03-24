open System.IO


let rec runRec (position: int) (input: int[]) =
  match input.[position] with
    | 99 -> input
    | 1 ->
      let savePosition = input.[position+3]
      let opResult = input.[input.[position+1]] + input.[input.[position+2]]
      runRec (position+4) (input |> Array.updateAt savePosition opResult)
    | 2 ->
      let savePosition = input.[position+3]
      let opResult = input.[input.[position+1]] * input.[input.[position+2]]
      runRec (position+4) (input |> Array.updateAt savePosition opResult)
    | _ -> raise (System.ArgumentException("Invalid opcode"))


let run (input:int[]) (noun:int, verb:int) =
  runRec 0 (input |> Array.updateAt 1 noun |> Array.updateAt 2 verb)


let input =
  "input-day-02.txt"
  |> File.ReadAllLines
  |> Seq.head
  |> (fun x -> x.Split(","))
  |> Seq.map(fun x -> int x)
  |> Seq.toArray

let result =
  Seq.allPairs { 0 .. 99 } { 0 .. 99 }
  |> Seq.map(fun x -> (x, (run input x).[0]))
  |> Seq.filter(fun (x,y) -> y = 19690720)
  |> Seq.map(fun ((noun, verb),y) -> 100 * noun + verb)

printfn "%A" result