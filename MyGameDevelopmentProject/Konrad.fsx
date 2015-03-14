//let getSpillere = spillerListe textContent

let res = [(1,2); (3,2); (4,2);(5,2);(1,2)]
let res1 = [for i in [1..9] do
                for j in [i..3]-> (i,j)]

let swap (a,b) = (b,a)

let rec countPair pair = function
    |[] -> 0
    |h::t -> if h = pair || h = swap pair then 1 + countPair pair t else countPair pair t

let countTest = countPair (2,1) res

let canBePaired pair reslist players = 
    let (a,motst) = pair
    match countPair pair reslist with
    |0 -> true
    |1 -> 
        let rec loop list =
            match list with 
            | [] -> true 
            | h::t ->           
                if countPair (a,h) reslist < 1 && h<motst then false 
                else loop t
        loop (players|>List.filter(fun score -> score > a - 1))
    |_ -> false

let canTest = canBePaired (2,4) res [1..6]

let failedPair pair list = 
    list|>List.exists(fun res -> pair = res || pair = swap res)
     
let pairIt list = 
    let rec loop acc = function    
        |a::b::rest -> loop ((a,b)::acc) rest
        |_ -> acc
    loop [] list

let konrad players resultater =
    let ant = List.length players
    let rec loop spillere acc tried failed =
        match spillere with
        |a::b::rest ->
            if failedPair (a,b) resultater || failedPair (a,b) failed then                
                loop (a::rest) acc (b::tried) failed
            else 
                let players = tried@rest |>List.sort                
                let failed = tried|>List.map(fun e -> a,e)
                loop players (a::b::acc) [] failed 
        |[p] -> // player p can't be paired
            match acc with
            |[a;b] -> // kun et par er satt opp til nå
                let players = (a::b::p::tried)|>List.sort
                let failedFiltered = (a,b)::failed|>List.filter(fun (p1,p2) -> p1=a || p2=a )                
                loop players [] [] failedFiltered
            
            |a::b::pairs ->
                let players = (a::b::p::tried)|>List.sort
                let failedSorted =  [for player in a..b -> (player,a)]                
                loop players pairs [] failedSorted
            
            |_ -> failwith "Not possible to pair players"
        
        |[] ->  acc |> pairIt

    loop players [] [] []
           
let konradTest = konrad [1..10] res1

