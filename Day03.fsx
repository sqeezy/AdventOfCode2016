open System.IO
let input = File.ReadAllText "Day03.txt"

// let input = """101 301 501
// 102 302 502
// 103 303 503
// 201 401 601
// 202 402 602
// 203 403 603"""

type Triangle = {A:int; B:int; C:int}

let triangle (s:string[]) = {
                                A = s.[0] |> int
                                B = s.[1] |> int
                                C = s.[2] |> int
                            }

let splitNumbers (s : string) = s.Split([|' '|])
                                    |> Array.filter (fun s -> s<>"")

let splitLinewise (s:string) = s.Split [|'\n'|] 

let parseLogicPartOne = splitLinewise 
                        >> Array.map splitNumbers 
                        >> Array.map triangle  

let variations t = [
                        (t.A, t.B, t.C)
                        (t.C, t.A, t.B)
                        (t.B, t.C, t.A)
                   ] 

let isValid (x1,x2,x3) = x1+x2 > x3

let triangleValid = variations >> Seq.map isValid >> Seq.fold (&&) true

let resultPartOne = input
                    |> parseLogicPartOne
                    |> Seq.filter triangleValid
                    |> Seq.length