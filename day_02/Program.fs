open System.IO


let getSum (i:int) (j:int) (array:int[]) =
  array.[i] + array.[j]


let getProduct(i:int) (j:int) (array:int[]) =
  array.[i] * array.[j]


let getOperation opcode =
  match opcode with
  | 1 -> getSum
  | 2 -> getProduct
  | _ -> raise (System.ArgumentException("Invalid opcode"))


let input =
  File.ReadAllLines("input-day-02.txt")
  |> Seq.head
  |> (fun x -> x.Split(","))
  |> Seq.map(fun x -> int x)
  |> Seq.toArray

let run (input:int[]) (noun:int, verb:int) =
  let output = Array.copy(input)
  output.[1] <- noun
  output.[2] <- verb

  let mutable position = 0

  while (output.[position] <> 99) do
    let operation = getOperation output.[position]
    let savePosition = output.[position+3]
    output.[savePosition] <- operation output.[position+1] output.[position+2] output
    position <- position + 4;
  output


let result =
  Seq.allPairs { 0 .. 99 } { 0 .. 99 }
  |> Seq.map(fun x -> (x, (run input x).[0]))
  |> Seq.filter(fun (x,y) -> y = 19690720)
  |> Seq.map(fun ((noun, verb),y) -> 100 * noun + verb)
printfn "%A" result