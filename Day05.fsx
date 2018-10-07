open System.Security.Cryptography
open System.Text
open System

let md5 (data : byte array) : string =
    use md5 = MD5.Create()
    (StringBuilder(), md5.ComputeHash(data))
    ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
    |> string

let personalInput = "wtnhxymk"

let hashWithSalt input (salt:int) = (input + salt.ToString()) 
                                 |> System.Text.Encoding.ASCII.GetBytes 
                                 |> md5

let personalHash = hashWithSalt <| personalInput

let isInterestingHash (hash:string) = hash.StartsWith("00000")

let saveSixthChar hash state = Seq.item 5 hash :: state

let stateIsFinal state = Seq.length state > 7

let solver isMatch saveToState isFinished i state =
    let rec solve i state = 
        let hash = personalHash i
        match isMatch hash with
        | true ->
            let newChars = saveToState hash state
            match isFinished newChars with
            | true  -> List.rev newChars
            | false -> solve (i+1) newChars
        | false -> solve (i+1) state
    solve i state

let solverPartOne = solver isInterestingHash saveSixthChar stateIsFinal 

let resultPartOne = solverPartOne 0 List.Empty |> String.Concat
