module day_05

open NUnit.Framework
open System
open System.IO

let getValue (input: int[]) (mode, position) =
    match mode with
    | 0 -> input.[input.[position]]
    | 1 -> input.[position]
    | _ -> raise (NotSupportedException())

let getArgCount intCode =
    match intCode with
    | 1 -> 3
    | 2 -> 3
    | 3 -> 1
    | 4 -> 1
    | 99 -> 0
    | _ -> raise (NotSupportedException())

// Challenge 1: How to parse the opcode?
let parseIntCodeAndModes (input: int) =
    let intCode = input % 100
    let numArgs = getArgCount intCode
    let remainingArgs = input / 100
    let parsedModes =
        (numArgs, remainingArgs)
        |> Seq.unfold(fun (remaining, state) ->
            if remaining > 0 then Some (state % 10, (remaining-1, state/10))
            else None)
        |> Seq.toList
    [intCode] @ parsedModes

let rec runRec (position: int) (input: int[]) =
  match parseIntCodeAndModes input.[position] with
    | [99] -> input
    | 1::[mode1; mode2; _]->
      let arg1 = getValue input (mode1, position+1)
      let arg2 = getValue input (mode2, position+2)
      let arg3 = getValue input (1, position+3)
      let opResult = arg1 + arg2
      runRec (position+4) (input |> Array.updateAt arg3 opResult)
    | 2::[mode1; mode2; _]->
      let arg1 = getValue input (mode1, position+1)
      let arg2 = getValue input (mode2, position+2)
      let arg3 = getValue input (1, position+3)
      let opResult = arg1 * arg2
      runRec (position+4) (input |> Array.updateAt arg3 opResult)
    | [3; _] ->
      let userInput = Console.ReadLine() |> int
      let arg = getValue input (1, position+1)
      runRec (position+2) (input |> Array.updateAt arg userInput)
    | [4; mode1] ->
      let arg = getValue input (mode1, position+1)
      printfn "Diagnostic code: %d" arg
      runRec (position+2) input
    | n ->
        printfn "Opcode: %A" n
        raise (ArgumentException("Invalid opcode"))

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``Parse correctly intcode with modes``() =
    let output = parseIntCodeAndModes 1002
    let expected = [2; 0; 1; 0]
    Assert.That (output, Has.Exactly(4).Items)
    Assert.That (output, Is.EqualTo (expected), "The output and expected are not equal")

[<Test; Ignore("Not wanted for now")>]
let Test1 () =
    let input =
      "../../../input-day-05.txt"
      |> File.ReadAllLines
      |> Seq.head
      |> (fun x -> x.Split(","))
      |> Seq.map(fun x -> int x)
      |> Seq.toArray
    Console.WriteLine(input.Length)
    runRec 0 input |> ignore
    Assert.Pass()
