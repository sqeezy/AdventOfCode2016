// let flatInput = """R8, R4, R4, R8""" 
let flatInput = """L3, R1, L4, L1, L2, R4, L3, L3, R2, R3, L5, R1, R3, L4, L1, L2, R2, R1, L4, L4, R2, L5, R3, R2, R1, L1, L2, R2, R2, L1, L1, R2, R1, L3, L5, R4, L3, R3, R3, L5, L190, L4, R4, R51, L4, R5, R5, R2, L1, L3, R1, R4, L3, R1, R3, L5, L4, R2, R5, R2, L1, L5, L1, L1, R78, L3, R2, L3, R5, L2, R2, R4, L1, L4, R1, R185, R3, L4, L1, L1, L3, R4, L4, L1, R5, L5, L1, R5, L1, R2, L5, L2, R4, R3, L2, R3, R1, L3, L5, L4, R3, L2, L4, L5, L4, R1, L1, R5, L2, R4, R2, R3, L1, L1, L4, L3, R4, L3, L5, R2, L5, L1, L1, R2, R3, L5, L3, L2, L1, L4, R4, R4, L2, R3, R1, L2, R1, L2, L2, R3, R3, L1, R4, L5, L3, R4, R4, R1, L2, L5, L3, R1, R4, L2, R5, R4, R2, L5, L3, R4, R1, L1, R5, L3, R1, R5, L2, R1, L5, L2, R2, L2, L3, R3, R3, R1""" 

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
              History: (int*int) list;}

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

let positionInDistance (x,y) bearing distance=
    match bearing with
    |North -> (x, y+distance)
    |East  -> (x+distance,y)
    |South -> (x, y-distance)
    |West  -> (x-distance, y)

let move start bearing stepSize : (int*int) list=
    [stepSize .. -1 .. 1] 
        |> List.map (positionInDistance start bearing)

let nextStep s =
    match s.Moves with
    |[] -> None
    |(turn,stepSize)::ms ->
        let {Position=(x,y) as pos;Bearing=b;History=hist} = s 
        let newBearing =
            match turn with
            |Left  -> turnLeft b
            |Right -> turnRight b
        let steps = move pos newBearing stepSize
        let newHistory = steps @ hist
        let newPosition= List.head steps
        let newState = {Position=newPosition; Bearing=newBearing;Moves=ms;History=newHistory}
        Some newState



let initial = {Position=(0,0);Bearing=North;Moves=moves;History=List.singleton (0,0)}

let rec solvePartOne s=
    let next = nextStep s
    match next with
    |Some n -> solvePartOne n
    |None -> s

let length {Position=(x,y)} =
     System.Math.Abs x + System.Math.Abs y
let resultPartOne = solvePartOne initial |> length


let firstCollision history = history
                                |> List.groupBy (fun x-> x)
                                |> List.filter (fun (_,v) -> v.Length > 1)
                                |> List.tryHead
let rec solvePartTwo ({History=h} as s)=
    match firstCollision h with
    |Some (pos , _) -> {s with Position=pos}
    |None _ -> 
            let next = nextStep s
            match next with
            |Some n -> solvePartTwo n
            |None -> s

let resultPartTwo = solvePartTwo initial |> length