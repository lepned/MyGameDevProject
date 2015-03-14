namespace Game

open System
open Game.Domain

module Rendering =

    let offset (pos:Pos) = (pos.Left, pos.Top + 2) // for å gjøre plass til score på posisjon 0,0 i hovedvinduet...
    
    let initializeDisplay (size:Size) (state:GameState) =
        Console.SetWindowSize(size.Width, size.Height+2)
        let board = state.Board
        for x in 0 .. (size.Height - 1) do
            for y in 0 .. (size.Width - 1) do
                let pos = { Top = x; Left = y }
                Console.SetCursorPosition (offset pos)
                let tileType = board.[x,y]
                match tileType with
                | Available -> 
                    Console.ForegroundColor <- ConsoleColor.Black
                    Console.Write(" ")                
                | Treasure ->
                    Console.ForegroundColor <- ConsoleColor.Green
                    Console.Write("$")
                | Trap -> 
                    Console.ForegroundColor <- ConsoleColor.Red
                    Console.Write("#")
                         
    let renderScore score = 
        Console.ForegroundColor <- ConsoleColor.White
        Console.SetCursorPosition (0,0)
        Console.Write (sprintf "Score: %i   " score)

    let updateDisplay (before:GameState) (after:GameState) =

        renderScore after.Score

        let oldPos = before.Creature.Position
        let newPos = after.Creature.Position
        // previous player position
        Console.SetCursorPosition (offset (oldPos))
        Console.ForegroundColor <- ConsoleColor.Black 
        Console.Write("█") //removed cell is overwritten with black box as foreground color
        // current player position
        Console.SetCursorPosition (offset (newPos))
        Console.ForegroundColor <- ConsoleColor.Yellow 
        Console.Write("█") //drawing creature to display a yellow box as foreground color