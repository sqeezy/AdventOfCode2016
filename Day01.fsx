let flatInput = """R8, R4, R4, R8""" 
// let flatInput = """L3, R1, L4, L1, L2, R4, L3, L3, R2, R3, L5, R1, R3, L4, L1, L2, R2, R1, L4, L4, R2, L5, R3, R2, R1, L1, L2, R2, R2, L1, L1, R2, R1, L3, L5, R4, L3, R3, R3, L5, L190, L4, R4, R51, L4, R5, R5, R2, L1, L3, R1, R4, L3, R1, R3, L5, L4, R2, R5, R2, L1, L5, L1, L1, R78, L3, R2, L3, R5, L2, R2, R4, L1, L4, R1, R185, R3, L4, L1, L1, L3, R4, L4, L1, R5, L5, L1, R5, L1, R2, L5, L2, R4, R3, L2, R3, R1, L3, L5, L4, R3, L2, L4, L5, L4, R1, L1, R5, L2, R4, R2, R3, L1, L1, L4, L3, R4, L3, L5, R2, L5, L1, L1, R2, R3, L5, L3, L2, L1, L4, R4, R4, L2, R3, R1, L2, R1, L2, L2, R3, R3, L1, R4, L5, L3, R4, R4, R1, L2, L5, L3, R1, R4, L2, R5, R4, R2, L5, L3, R4, R1, L1, R5, L3, R1, R5, L2, R1, L5, L2, R2, L2, L3, R3, R3, R1""" 

type Turn = Left | Right
type Move = (Turn * int)

let  createMove (inst:string) = 
    let turn =
        match inst.[0] with
        |'R' -> Right
        |'L' -> Left
    let stepSize = inst.Substring(1) |> int
    (turn,stepSize)
    
let moves = flatInput.Split([|','|]) 
                |> Array.map (fun x -> x.Trim())
                |> Array.map createMove
                |> Array.toList

type Bearing =
    |North
    |East
    |South
    |West

type State = {Position:(int*int);
              Bearing:Bearing
              Moves:Move list;
              History: (int*int) Set;}

let turnLeft b =
    match b with
    |North -> West
    |East  -> North
    |South -> East
    |West  -> South

let turnRight b =
    match b with
    |North -> East
    |East  -> South
    |South -> West
    |West  -> North

let nextStep s =
    match s.Moves with
    |[] -> None
    |(turn,stepSize)::ms ->
        let {Position=(x,y) as pos;Bearing=b;History=hist} = s 
        let newHistory = Set.add pos hist
        let newBearing =
            match turn with
            |Left  -> turnLeft b
            |Right -> turnRight b
        let newPosition=
            match newBearing with
            |North -> (x, y+stepSize)
            |East  -> (x+stepSize,y)
            |South -> (x, y-stepSize)
            |West  -> (x-stepSize, y)
        let newState = {Position=newPosition; Bearing=newBearing;Moves=ms;History=newHistory}
        Some newState



let initial = {Position=(0,0);Bearing=North;Moves=moves;History=Set.empty}

let rec solvePartOne s=
    let next = nextStep s
    match next with
    |Some n -> solvePartOne n
    |None -> s

let length {Position=(x,y)} =
     System.Math.Abs x + System.Math.Abs y
let resultPartOne = solvePartOne initial |> length


let rec solvePartTwo ({History=h} as s)=
    let next = nextStep s
    match next with
    |Some n -> 
        match n.History.Count with
        |c when c=h.Count -> n
        | _ -> solvePartTwo n
    |None -> s

let resultPartTwo = solvePartTwo initial