open System.IO
open System

let (|Int|_|) (s:string) =
    match Int64.TryParse s with
    | true,value -> Some value
    | false,_ -> None

let print (a:'a)= printfn "%A" a

let input = File.ReadAllLines "/tmp/aoc/input" |> Array.toList

let parseLine (line:string) =
    let line = line.Split [|' '|] |> Array.toList
    let pair (a:string) =
        let a = a.Split [|','|]
        (a[0] |> int,a[1] |> int)
    let rec parse (s:string list) =
        match s with
        | [] -> []
        | a::"->"::rest -> (pair a)::(parse rest)
        | [a] -> [pair a]
    parse line 

let lines = input |> List.map parseLine

let rec rocks (lines:(int*int) list) =
    printfn $"rocks({lines})"
    let scaleTo1 (i:int) = if i > 0 then 1 elif i = 0 then 0 else -1
    match lines with
    | [a] -> [a]
    | a::b::rest when a = b -> rocks (b::rest)
    | (x1,y1)::(x2,y2)::rest -> 
        let dx = x2-x1 |> scaleTo1
        let dy = y2-y1 |> scaleTo1
        print (dx,dy)
        (x1,y1) :: rocks ((x1+dx,y1+dy)::(x2,y2)::rest)
        
let blocked = lines
                |> List.map rocks
                |> List.concat
                |> Set.ofList
                
blocked.Count |> print

let depth = blocked |> Seq.toList |> List.map snd |> List.max 

let rec moveSand (sand:int*int) (blocked:Set<int*int>) =
    let available pos = blocked.Contains pos |> not 
    match sand with 
    | (x,y) when y > depth -> (x,y)
    | (x,y) when available (x,y+1) -> moveSand (x,y+1) blocked 
    | (x,y) when available (x-1,y+1) -> moveSand (x-1,y+1) blocked
    | (x,y) when available (x+1,y+1) -> moveSand (x+1,y+1) blocked
    | pos -> pos 

let initGrain = (500,0)

moveSand initGrain blocked |> print

printfn "Solving 1"

let rec fill (blocked:Set<int*int>) (sandpile:Set<int*int>) =
    let pos = moveSand initGrain blocked
    printfn $"fill {pos} {sandpile}"
    if pos = initGrain then sandpile 
//    elif snd pos > depth then sandpile
    else fill (blocked.Add pos) (sandpile.Add pos)
        
let sandPile = fill blocked Set.empty 

printfn "X"
    
printfn $"sand pile = {sandPile}"

let totalDepth = sandPile |> Set.toList |> List.map snd |> List.sum 

printfn $"total = {sandPile.Count} moves ={totalDepth}"

for y in 0..12 do
    for x in 490..510 do
        let pos = (x,y)
        if blocked.Contains pos then
            printf "#"
        elif sandPile.Contains pos then
            printf "o"
        else
            printf "."
    printfn ""

// lotsa debug print + a xmas tree + you need to add 1 to the answer...