namespace Game

open System
open System.Threading
open Game.Domain
open Game.Brain
open Game.Rendering

module Program = 
    
    [<EntryPoint>]
    let main argv = 

        let rng = Random()
        
        let size = 
            { Width = 120
              Height = 80 }
        
        let board = 
            Array2D.init size.Height size.Width (fun top left -> 
                let dice = rng.NextDouble ()
                if dice < 0.25 then Trap
                elif dice < 0.50 then Treasure
                else Available)
        
        let startPos = 
            { Top = size.Height / 2
              Left = size.Width / 2 }
        
        let critter = 
            { Position = startPos
              Direction = North }
        
        let startState = 
            { Board = board
              Creature = critter
              Score = 0 }
        
        
        let moreTresaures arr () = arr |> Seq.cast<Cell> |>Seq.tryPick(fun cell -> if cell=Treasure then Some cell else None)
        
        initializeDisplay size startState
        let move = applyDecision size
        
        let rec loop (state : GameState, brain : Brain) = 
            //let initBoards
            //rotate players
            //findColors players
            
            let makePlayers n = [for p in 1 .. n -> Spiller p]           
            let delOpp list =
                let lengde = List.length list
                list
                |>List.partition(fun item -> 
                    match item with
                    |Spiller n -> if n<=lengde/2 then true else false
                    |WO -> false)
                |>fun (lav,høy) -> List.zip lav (høy|>List.rev)

            let rec rotateEven list =                
                let lengde = list|>List.length
                if lengde%2=1 then rotateEven (WO::list)
                else
                list 
                |>List.mapi(fun idx item -> 
                    if idx=0 then 0,item
                    elif idx + 1 < lengde then idx+1,item
                    else idx + 2 - lengde, item)
                |>List.sortBy fst
                |>List.map snd

            let visible = visibleBy size state.Creature state.Board
            let decision = Brain.decide brain visible //Brain.randomDecide ()
            //world update
            let creature = state.Creature |> move decision
            let board = updateBoard state.Board creature
            let gain = Domain.computeGain state.Board creature
            let score = state.Score + gain
            let updatedState = 
                { Board = board
                  Creature = creature
                  Score = score }
            
            updateDisplay state updatedState
            //learning
            let nextVisible = visibleBy size creature board
            let experience = 
                { State = visible
                  Action = decision
                  Reward = float gain
                  NextState = nextVisible }
            
            let updatedBrain = learn brain experience
           
            Thread.Sleep 20
            match moreTresaures board () with
            |Some item -> 
                loop (updatedState, updatedBrain)
            |None -> 
                printfn "%A" " Done"
                System.Console.ReadLine () |>ignore

        loop (startState, Map.empty)

        0 // return an integer exit code
