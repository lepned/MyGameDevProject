namespace Game

module Domain =
    
    type Player =
        |WO
        |Spiller of int

    type Pos = {Top:int; Left:int}

    type Act =
        |Left
        |Right
        |Straight

    type Dir =
        |North
        |West
        |South
        |East

    type Creature = {Position:Pos; Direction:Dir}

    type Cell = 
        |Treasure
        |Trap
        |Available

    type Size = {Width:int; Height:int}

    type Board = Cell [,]

    type GameState = {Board:Board; Creature:Creature; Score:int}

    let inline (%%%) x y =
        if x >= 0 then x % y 
        else y + (x % y) 

    let onboard size pos =
        { Top = pos.Top %%% size.Height;
          Left= pos.Left %%% size.Width }

    let moveTo size dir pos =
        match dir with
        |North -> {pos with Top = pos.Top - 1 }
        |South -> {pos with Top = pos.Top + 1 }
        |West -> {pos with Left = pos.Left - 1 }
        |East -> {pos with Left = pos.Left + 1 }
        |> onboard size

    let goto act dir =
        match act with
        |Straight -> dir
        |Left ->
            match dir with
            |North -> West
            |West -> South
            |South -> East
            |East -> North
        
        |Right ->
            match dir with
            |North -> East
            |East -> South
            |South -> West
            |West -> North


    let applyDecision size action creature =
        let direction = creature.Direction |> goto action
        {Position=creature.Position |> moveTo size direction ; Direction=direction }


    let updateBoard board creature =
        let pos = creature.Position
        board |> Array2D.mapi(fun top left item -> if top = pos.Top && left = pos.Left then Available else item )
        

    let computeGain (board:Board) creature =
        let pos = creature.Position
        let tileType =board.[pos.Top,pos.Left]
        match tileType with
        |Treasure -> 100
        |Trap -> -100
        |Available -> 0
