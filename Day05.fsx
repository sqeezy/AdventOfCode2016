open System.Security.Cryptography
open System.Text
open System

let md5 (data : byte array) : string =
    use md5 = MD5.Create()
    (StringBuilder(), md5.ComputeHash(data))
    ||> Array.fold (fun sb b -> sb.Append(b.ToString("x2")))
    |> string


let hashWithSalt input (salt:int) = (input + salt.ToString()) 
                                 |> System.Text.Encoding.ASCII.GetBytes 
                                 |> md5

let personalInput = "wtnhxymk"
let personalHash = hashWithSalt <| personalInput

//general solver
let solver isMatch saveToState isFinished i state =
    let rec solve i state = 
        let hash = personalHash i
        match isMatch hash with
        | true ->
            let newChars = saveToState hash state
            match isFinished newChars with
            | true  -> Seq.rev newChars
            | false -> solve (i+1) newChars
        | false -> solve (i+1) state
    solve i state

//part one constraints for solver
let isInterestingHash (hash:string) = hash.StartsWith("00000")
let saveSixthChar hash state = Seq.item 5 hash :: state
let pwIsLongEnough state = Seq.length state > 7

//use part one logic
let solverPartOne = solver isInterestingHash saveSixthChar pwIsLongEnough 
let resultPartOne = solverPartOne 0 List.Empty |> String.Concat
