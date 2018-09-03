open System.Security.Cryptography
open System.Text
open System

let md5 (data : byte array) : string =
    use md5 = MD5.Create()
    (StringBuilder(), md5.ComputeHash(data))
    ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
    |> string

let input = "wtnhxymk"

let hashWithSalt (salt:int) = (input + salt.ToString()) 
                                 |> System.Text.Encoding.ASCII.GetBytes 
                                 |> md5

let isInterestingHash (hash:string) =
    match hash.StartsWith("00000") with
    | true -> Some hash.[5]
    | false -> None

let rec findPassword i (chars:char list) =
    let hash = hashWithSalt i
    match isInterestingHash hash with
    | Some char ->
        let newChars = char::chars
        match newChars.Length with
        | x when x > 7 -> List.rev newChars
        | _            -> findPassword (i+1) newChars
    | None -> findPassword (i+1) chars

let resultPartOne = findPassword 0 List.Empty |> String.Concat