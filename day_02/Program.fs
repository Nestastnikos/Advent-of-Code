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

let run (noun:int) (verb:int) (input:int[]) =
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

printfn "%A" input