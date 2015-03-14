#r """C:\Users\lepne_000\documents\visual studio 2013\Projects\MyGameDevelopmentProject\packages\Newtonsoft.Json.6.0.6\lib\net45\Newtonsoft.Json.dll"""

open Newtonsoft.Json
open System.IO

type Player =
    |WO
    |Spiller of int
 
type Result = WhiteWon |WhiteLost |Draw |Ingen
 
type Bord = {Id:int; Hvit:Player; Sort:Player; Result:Result}
 
type Runde = {Id:int; AlleBord:Bord list}
 
type Berger = {Dato:System.DateTime; Runder: Runde list; Spillere: Player list}
 
let swap (a,b) = (b,a)
let rng = System.Random()
 
let setColor runde bord pair =
    match bord % 2 = 1 with
    |true when bord=1 -> if runde % 2 = 1 then pair else swap pair
    |true -> pair
    |_ -> swap pair
 
let initPlayers n = 
    let init = [for p in 1 .. n -> Spiller p]
    if (init|>List.length) % 2 = 1 then init@[WO] else init
 
let rotate list =
    let arr = List.toArray list
    let lengde = arr.Length-1
    [   yield arr.[lengde - 1]
        yield! arr.[0 ..lengde - 2]
        yield arr.[lengde]]
 
let getPairs list =
    let lengde = list|>List.length
    [for i=0 to list.Length/2-1 do yield list.[i],list.[lengde-i-1] ]
 
let getRundeOppsett runde list =    
    let rec loop n lst =
        match n with
        |1 -> lst
        |_ -> loop (n-1) (rotate lst)    
    let boards=
        getPairs (loop runde list)
        |>List.mapi (fun idx pair -> idx+1,pair)
        |>List.map (fun (bordId,pair) -> 
            let w,b=setColor runde bordId pair
            {Id=bordId;Hvit=w;Sort=b;Result=Ingen})
    {Id=runde;AlleBord=boards}
 
let bergerRunder n =
    let players = initPlayers n
    let antrunder = players.Length-1
    let runder = [for i in 1 .. antrunder -> getRundeOppsett i players]
    let dato=System.DateTime.Now
    {Dato=dato;Runder=runder; Spillere=players}
 
let countColors (berger:Berger) =
    let coll = berger.Runder |> List.collect (fun runde -> runde.AlleBord)
    let whites = coll |>Seq.countBy (fun bord -> bord.Hvit)|>Seq.sort |> Seq.toList
    let blacks = coll |>Seq.countBy (fun bord -> bord.Sort) |>Seq.sort |> Seq.toList
    List.zip whites blacks |> List.map (fun (w,b) -> sprintf "%A: Hvit %i Sort %i" (fst w) (snd w) (snd b))
 
let berger = bergerRunder 4
 
let updateResult runde =
    let result = [WhiteWon;WhiteLost;Draw]
    let bordene =
        [for bord in runde.AlleBord do
            let dice = result.[rng.Next(result.Length)]
            match bord.Hvit,bord.Sort with
            |WO,_ -> yield {bord with Result = WhiteLost}
            |_,WO -> yield {bord with Result = WhiteWon}
            |_ ->    yield {bord with Result = dice}]        
    {runde with AlleBord=bordene}
 
let bergerWithResults (berger:Berger) = 
    let rec loop list =
        match list with
        |[] -> []
        |h::t ->
            updateResult h :: loop t
    loop berger.Runder
 
let result = bergerWithResults berger

//json og read/write to file
let path = """C:\Users\lepne_000\Documents\JsonTest.json"""

let jsonOutput = JsonConvert.SerializeObject(berger)
let fromJson = JsonConvert.DeserializeObject<Berger>(jsonOutput)
let writeBerger path (values:Berger) = 
    let text = JsonConvert.SerializeObject(values)
    File.WriteAllText(path,text)

let readBerger path =
    let text = File.ReadAllText(path)
    JsonConvert.DeserializeObject<Berger>(text)

writeBerger path berger
readBerger path

//let colorCheck = countColors berger