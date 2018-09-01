open System.IO

let input = File.ReadAllLines "Day04.txt" |> Array.toList

let splitIntoParts (s:string) = 
    let parts =s.Split [|'-'|] |> Array.toList
    let length = List.length parts
    let name = parts |> List.take (length-1) |> List.fold (+) ""

    let last = List.last parts

    let id::checksum = last.Split [|'[';']'|]
                                |> Array.toList
                                |> List.filter (fun s -> s<>"") 

    (name, id |> int, checksum.Head)

let countChar map c =
    match Map.containsKey c map with
    | true -> Map.add c (map.[c]+1) map
    | false -> Map.add c 1 map

let actualChecksum (name:string) =
    let countMap = name |> Seq.toList |> Seq.fold countChar Map.empty
    countMap 
        |> Map.toSeq 
        |> Seq.map fst 
        |> Seq.sortByDescending (fun c -> countMap.[c]) 
        |> Seq.take 5
        |> (Seq.map string >> String.concat "")

let isValidRow (name:string, _, checksum:string) =
    actualChecksum name = checksum

let resultPartOne = input 
                    |> List.map splitIntoParts
                    |> List.filter isValidRow
                    |> List.map (fun (_,id,_) -> id)
                    |> List.sum