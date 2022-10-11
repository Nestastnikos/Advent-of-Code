module day_04

open NUnit.Framework
open System.Text.RegularExpressions
open System.IO

let increase ch =
    let next =
        match ch with
        | '0' -> '1'
        | '1' -> '2'
        | '2' -> '3'
        | '3' -> '4'
        | '4' -> '5'
        | '5' -> '6'
        | '6' -> '7'
        | '7' -> '8'
        | '8' -> '9'
        | '9' -> '0'
    (next, ch = '9')

let regex = new Regex(@"([0-9])\1", RegexOptions.Compiled)

let isIncreasing (input:string) =
    input.ToCharArray()
    |> Array.map (fun x -> int x)
    |> Array.pairwise
    |> Array.map (fun (a,b) -> a <= b)
    |> Array.reduce (fun a b -> a && b)

let findNextUpper input =
    let length = input |> String.length
    let rec findNextUpperRec index =
        match index with
        | 0 ->
            match regex.IsMatch input && isIncreasing input with
            | true ->
                let next, _ = increase input.[0]
                String.replicate length (string next)
            | false ->
                String.replicate length (string input.[0])
        | i ->
            let next, isCarry = increase input.[i]
            let newString =
                match isCarry with
                | true ->
                    findNextUpperRec (i-1)
                | false ->
                    input.[0..i-1] + String.replicate (length - i) (string next)
            match regex.IsMatch newString && isIncreasing input with
            | false ->
                findNextUpperRec (index-1)
            | true ->
                newString
    findNextUpperRec (length-1)

let findNextLower input =
    let rec findNextLowerRec index =
        let length = input |> String.length
        match int input.[index] > (int input.[index+1]) with
        | true ->
            match index with
            | 0 ->
                String.replicate 2 (string input.[0]) + String.replicate (length - index - 1) "9"
            | n ->
                input.[0..index-1] + (string input.[index-1]) + String.replicate (length - index - 1) "9"
        | false ->
            findNextLowerRec (index+1)
    findNextLowerRec 0

[<SetUp>]
let Setup () =
    ()

[<Test>]
let findNextLowerTest () =
    let input = "785961"
    let result = findNextLower input
    Assert.AreEqual("779999", result)

// [<Test>]
let part1 () =
    let lowerBound = "271973"
    let lowerBound = "269999"
    let upperBound = "785961"

    let upperBoundSearched = findNextLower upperBound

    let result =
        lowerBound
        |> Seq.unfold (fun prev ->
            match prev = upperBoundSearched with
            | false ->
                let next = findNextUpper prev
                Some(next, next)
            | true ->
                None
            )
        |> Seq.toList
    File.WriteAllText("tmp.txt", String.concat "\n" result)
    Assert.AreEqual(result, 1675)

// [<TestCase("100000", "111111")>]
// [<TestCase("111111", "111112")>]
// [<TestCase("111112", "111113")>]
// [<TestCase("111199", "111222")>]
// [<TestCase("299999", "333333")>]
// [<TestCase("779999", "788888")>]
let findNextUpperWorksTest input output =
    Assert.AreEqual(output, findNextUpper input)
