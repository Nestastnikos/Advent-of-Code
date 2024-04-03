module Tests

open Xunit
open System.IO

//
// Implementation Part 1
//
let getFuel value =
    (value / 3) - 2

[<Fact>]
let ``Part 1 Test Case 01`` () =
    Assert.Equal(getFuel 12, 2)

[<Fact>]
let ``Part 1 Test Case 02`` () =
    Assert.Equal(getFuel 14, 2)

[<Fact>]
let ``Part 1 Test Case 03`` () =
    Assert.Equal(getFuel 1969, 654)

[<Fact>]
let ``Part 1 Test Case 04`` () =
    Assert.Equal(getFuel 100756, 33583)

[<Fact>]
let ``Part 1``() =
    let result =
        File.ReadAllLines("../../../input.txt")
        |> Array.map (fun x -> x |> int |> getFuel)
        |> Array.sum
    Assert.Equal(3331849, result)

//
// Implementation Part 2
//
let rec getFuelRec value =
    match getFuel value with
    | x when x > 0 -> x + getFuelRec x
    | _ -> 0

[<Fact>]
let ``Part 2 Test Case 01`` () =
    Assert.Equal(getFuelRec 14, 2)

[<Fact>]
let ``Part 2 Test Case 02`` () =
    Assert.Equal(getFuelRec 1969, 966)

[<Fact>]
let ``Part 2 Test Case 03`` () =
    Assert.Equal(getFuelRec 100756, 50346)

[<Fact>]
let ``Part 2``() =
    let result =
        File.ReadAllLines("../../../input.txt")
        |> Array.map (fun x -> x |> int |> getFuelRec)
        |> Array.sum
    Assert.Equal(4994898, result)
