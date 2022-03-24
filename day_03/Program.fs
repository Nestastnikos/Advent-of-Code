open System.IO
open System

let getNextPoint direction length (x, y) =
    match direction with
    | 'R' -> (x+length, y)
    | 'L' -> (x-length, y)
    | 'U' -> (x, y+length)
    | 'D' -> (x, y-length)
    | _ -> raise (ArgumentException("Invalid direction"))


let intersects x11 x12 x21 x22 =
  let (xmin, xmax) = min x11 x12, max x11 x12
  (xmin <= x21 && x21 <= xmax) || (xmin <= x22 && x22 <= xmax)


let getValue a b c =
  match a = b with
  | true -> a
  | false -> c


let findIntersection (x11, y11, x12,y12, x21,y21) =
  let x  = getValue x11 x12 x21
  let y  = getValue y11 y12 y21
  (x, y)

let wires =
    "full-input.txt"
    |> File.ReadAllLines
    |> Seq.map (fun x -> x.Split ",")
    |> Seq.map (fun x ->
        x
        |> Seq.map (fun y -> (y.[0], int y.[1..]))
        |> Seq.scan
            (fun line (direction:char,length:int) ->
              getNextPoint direction length line)
            (0,0)
        |> Seq.pairwise)

let wire1 = wires |> Seq.item 0
let wire2 = wires |> Seq.item 1

let result =
  Seq.allPairs wire1 wire2
  |> Seq.filter(fun (((x11,y11),(x12,y12)),((x21,y21),(x22,y22))) ->
    (intersects x11 x12 x21 x22 || intersects x21 x22 x11 x12) &&
    (intersects y11 y12 y21 y22 || intersects y21 y22 y11 y12))
  |> Seq.skip 1
  |> Seq.map(fun(((x11,y11),(x12,y12)),((x21,y21),(x22,y22))) -> x11,y11,x12,y12,x21,y21)
  |> Seq.map(fun x -> findIntersection x)
  |> Seq.map(fun (x,y) -> x+y)
  |> Seq.min

printfn "%A" result
