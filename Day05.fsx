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

let isInterestingHash (hash:string) = hash.StartsWith("00000")

let saveSixthChar hash state = Seq.item 5 hash :: state

let stateIsFinal state = Seq.length state > 7

let rec findPassword i (state:char list) =
    let hash = hashWithSalt i
    match isInterestingHash hash with
    | true ->
        let newChars = saveSixthChar hash state
        match stateIsFinal newChars with
        | true  -> List.rev newChars
        | false -> findPassword (i+1) newChars
    | false -> findPassword (i+1) state


let rec findPassword2 isMatch saveToState isFinished i state =
    let hash = hashWithSalt i
    match isMatch hash with
    | true ->
        let newChars = saveToState hash state
        match isFinished newChars with
        | true  -> List.rev newChars
        | false -> findPassword (i+1) newChars
    | false -> findPassword (i+1) state

let solverPartOne = findPassword2 isInterestingHash saveSixthChar stateIsFinal 

let resultPartOne = solverPartOne 0 List.Empty |> String.Concat