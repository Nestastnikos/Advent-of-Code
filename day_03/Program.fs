open System.IO
open System

let wires =
  "full-input.txt"
  |> File.ReadAllLines
  |> Seq.map(fun input ->
    input.Split ","
    |> Seq.fold(fun (map, (x, y, d)) v ->
        let (dir, length) = v.[0], int v.[1..]
        let (xStep, yStep) =
          match dir with
          | 'R' -> (1,0)
          | 'L' -> (-1,0)
          | 'U' -> (0,1)
          | 'D' -> (0,-1)
          | _ -> raise (ArgumentException("Invalid direction"))

        let newMap =
          { 1 .. length }
          |> Seq.fold (fun acc n ->
            let (nextX, nextY, nextDist) = x+(n*xStep), y+(n*yStep), d+n
            let coord = (nextX, nextY)
            match acc |> Map.containsKey coord with
            | true ->
              acc
            | false ->
              acc |> Map.add coord nextDist
            )
            map

        let nextLastCoord = x+(length*xStep), y+(length*yStep), d+length
        (newMap, nextLastCoord)
      )
      (Map([]), (0,0,0))
  )
  |> Seq.map (fun (map,_) -> map)

let intersections =
  wires
  |> Seq.map (fun x -> x |> Map.keys |> Set.ofSeq)
  |> Set.intersectMany

// part 1
let minDistanceManh =
  intersections
  |> Set.map (fun (x,y) -> x+y)
  |> Set.minElement

// part 2
let minDistanceBoth =
  intersections
  |> Set.map (fun coord ->
    wires
    |> Seq.map (fun e -> e |> Map.find coord)
    |> Seq.reduce (fun a b -> a+b))
  |> Set.minElement

printfn "%d" minDistanceBoth