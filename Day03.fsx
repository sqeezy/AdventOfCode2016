open System.IO
let input = File.ReadAllLines("Day03.txt")

// let input = """ 324  768  190
//  985  309  356
//  41  491  802
//  997  793  905
//  976  684  837"""

type Triangle = {A:int; B:int; C:int}

let triangle (s:string[]) = {
                                A = s.[0] |> int
                                B = s.[1] |> int
                                C = s.[2] |> int
                            }

let splitNumbers (s : string) = s.Split([|' '|])
                                    |> Array.filter (fun s -> s<>"")

let lineToTriangle  =  splitNumbers >> triangle

let triangles = input
                    |> Array.map lineToTriangle
let variations t = [
                        (t.A, t.B, t.C)
                        (t.C, t.A, t.B)
                        (t.B, t.C, t.A)
                   ] 

let isValid (x1,x2,x3) = x1+x2 > x3

let triangleValid = variations >> Seq.map isValid >> Seq.fold (&&) true

let resultPartOne = triangles
                        |> Seq.filter triangleValid
                        |> Seq.length