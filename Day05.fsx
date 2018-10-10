open System.Security.Cryptography
open System.Text
open System
open System.Collections.Generic
open System.Collections.Generic

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
let solver isMatch saveToState isFinished postprocess i state =
    let rec solve i state = 
        let hash = personalHash i
        match isMatch hash with
        | true ->
            let newChars = saveToState hash state
            match isFinished newChars with
            | true  -> postprocess newChars
            | false -> solve (i+1) newChars
        | false -> solve (i+1) state
    solve i state

let isInterestingHash (hash:string) = hash.StartsWith("00000")

//part one parameters for solver
let saveSixthChar hash state = Seq.item 5 hash :: state
let pwIsLongEnough state = Seq.length state > 7

//part two parameters for solver
type Key = Value of int
let initial = new Dictionary<int, char>()

let parseIndex hash = 
    match Int32.TryParse(Seq.item 5 hash) with
    | (true,int) -> Some(int)
    | _ -> None

let placeInSolution position (state : Dictionary<int, char>) =
    match state.ContainsKey(position) with
    | true  -> state
    | false -> state.Add(position, 'c'); state

let saveSeventhAtPosition hash state =
    match parseIndex hash with
    | Some i -> placeInSolution i state
    | None   -> state

//use part one logic
let solverPartOne = solver isInterestingHash saveSixthChar pwIsLongEnough Seq.rev
let resultPartOne = solverPartOne 0 List.Empty |> String.Concat
