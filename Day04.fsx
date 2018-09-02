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

let validRows = input 
                |> List.map splitIntoParts
                |> List.filter isValidRow
let resultPartOne = validRows
                    |> List.map (fun (_,id,_) -> id)
                    |> List.sum

let shift (c:char) (i:int) = ((c |> int) - 96  + i) % 26
                             |> (+) 96 
                             |> char

let decriptSingleChar (i:int) (c:char) = 
    match c with
    | '-' -> ' '
    | x -> shift x i

let decriptString (s:string) (i:int) = s|> Seq.map (decriptSingleChar i) |> Seq.map string |> String.concat ""

let resultPartTwo = validRows 
                    |> List.map (fun(name , id, _) -> (decriptString name id, id))
                    |> List.filter (fun (deciphered, _) -> deciphered.Contains("north"))
                    |> List.map (fun (_, id) -> id)
                    |> List.head