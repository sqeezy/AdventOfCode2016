open System.IO
let input = File.ReadAllText "Day03.txt"

// let input = """101 301 501
// 102 302 502
// 103 303 503
// 201 401 601
// 202 402 602
// 203 403 603"""

type Triangle = {A:int; B:int; C:int}

let triangle (s:string list) = {
                                A = s.[0] |> int
                                B = s.[1] |> int
                                C = s.[2] |> int
                            }

let splitAtSpace (s : string) = s.Split([|' '|])
                                    |> Array.toList
                                    |> List.filter (fun s -> s<>"")

let splitLinewise (s:string) = s.Split [|'\n'|] |> Array.toList

let transpose (l:string list list) = [
                                        [l.[0].[0]; l.[1].[0]; l.[2].[0]]
                                        [l.[0].[1]; l.[1].[1]; l.[2].[1]]
                                        [l.[0].[2]; l.[1].[2]; l.[2].[2]]
                                     ]

let rec getVerticalTripletts  verticals lines : string list list =
    match List.length lines with
    | l when l > 2 ->
        let (threeLines,remainder) = List.splitAt 3 lines
        let transposed = transpose threeLines
        getVerticalTripletts (verticals @ transposed) remainder
    | _ -> verticals

let variations t = [
                        (t.A, t.B, t.C)
                        (t.C, t.A, t.B)
                        (t.B, t.C, t.A)
                   ] 

let isValid (x1,x2,x3) = x1+x2 > x3

let triangleValid = variations >> Seq.map isValid >> Seq.fold (&&) true

let parseLogicPartOne = splitLinewise 
                        >> List.map splitAtSpace
                        >> List.map triangle

let parseLogicPartTwo = splitLinewise
                        >> List.map splitAtSpace
                        >> getVerticalTripletts []
                        >> List.map triangle

let resultPartOne = input
                    |> parseLogicPartOne
                    |> Seq.filter triangleValid
                    |> Seq.length
let resultPartTwo = input
                    |> parseLogicPartTwo
                    |> Seq.filter triangleValid
                    |> Seq.length