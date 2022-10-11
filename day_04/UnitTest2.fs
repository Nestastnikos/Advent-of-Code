module day_04_part2

open NUnit.Framework
open System.Text.RegularExpressions
open System.IO

let regex = new Regex(@"([0-9])\1+", RegexOptions.Compiled)
let isAdjacent input =
    (regex.Matches input).GetEnumerator()
    |> Seq.unfold (fun state ->
        match state.MoveNext() with
        | true ->
            Some(string state.Current, state)
        | false ->
            None)
    |> Seq.exists (fun x -> String.length x = 2)

let isIncreasing (input:string) =
    input.ToCharArray()
    |> Array.map (fun x -> int x)
    |> Array.pairwise
    |> Array.map (fun (a,b) -> a <= b)
    |> Array.reduce (fun a b -> a && b)

let rec findNextUpper input =
    let next = string (int input+1)
    match isIncreasing next && isAdjacent next with
    | true ->
        next
    | false ->
        findNextUpper next


[<Test>]
let part2 () =
    let lowerBound = "271973"
    let upperBound = "785961"
    let upperBound = "779999"

    let result =
        lowerBound
        |> Seq.unfold (fun prev ->
            match prev = upperBound with
            | false ->
                let next = findNextUpper prev
                Some(next, next)
            | true ->
                None
            )
        |> Seq.toList
    File.WriteAllText("tmp.txt", String.concat "\n" result)
    Assert.AreEqual(result |> List.length, 1675)

[<TestCase("100000", "111122")>]
[<TestCase("111111", "111122")>]
[<TestCase("111122", "111133")>]
[<TestCase("111199", "111223")>]
[<TestCase("299999", "333344")>]
[<TestCase("779999", "788899")>]
let findNextUpperWorksTest input output =
    Assert.AreEqual(output, findNextUpper input)