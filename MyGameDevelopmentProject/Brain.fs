namespace Game
open System
open Game.Domain


module Brain =    

    type State = Dir * Cell list

    type Experience = {
        State: State;
        Action: Act;
        Reward: float;
        NextState: State; }

    type Strategy = { State:State; Action:Act; }

    type Brain = Map<Strategy,float>
    
    
    (* modelling state, strategy and brain *)

    let visibleBy size creature (board:Board) =
        creature.Direction,
        [ for t in -1 .. 1 do
            for l in -1 .. 1 -> // arrow instead of do means that you yield something instead of returning unit...
                board.[(creature.Position.Top + t) %%% size.Height, (creature.Position.Left + l) %%% size.Width]
        ]


    (*
TODO: implement learn, so that the creature
updates its brain based on the latest experience it had.
- create the Strategy corresponding to the State and
Decision from the latest Experience recorded,
- update the Strategy value as 
(1 - alpha) * V(strat) + alpha * reward from experience 
*)

    //random decision making
    let options = [|Straight;Left;Right|]
    let rng = Random()
    let randomDecide () = options.[rng.Next(options.Length)]
    
    //creature with brain
    let alpha = 0.2 // learning rate
    let epsilon = 0.05
    //let updateStrategy alpha stratVal reward = (1.0 - alpha) * stratVal + alpha * reward

    let learn (brain:Brain) (exp:Experience) =
        let strategy = {State = exp.State; Action = exp.Action }
        match brain.TryFind strategy with
        |Some value -> brain.Add(strategy, (1.0 - alpha) * value + alpha * exp.Reward)
        |_ -> brain.Add(strategy, exp.Reward * alpha)
       
    
(*
TODO: implement decide, so that the creature
uses its brain to make a decision, based on its
current state.
- if the state has never been seen before, 
make a random move,
- otherwise, among the known strategies that 
correspond to the current state, pick the strategy 
that has the highest value.
*)

    let decide (brain:Brain) (state:State) =
        if rng.NextDouble () < epsilon then
            randomDecide ()
        else
        let eval =
            options
            |>Array.map(fun act -> {State=state; Action=act} )
            |>Array.filter(fun strat -> brain.ContainsKey strat)
        
        match eval.Length with
        |0 -> randomDecide ()
        |_ ->
            options
            |>Array.maxBy(fun alt -> 
                let strat = {State=state;Action=alt}
                match brain.TryFind strat with
                |Some value -> value
                |None -> 0.0)
                    

