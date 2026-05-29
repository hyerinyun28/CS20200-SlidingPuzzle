module Shuffle

/// Creating an initial shuffled board

open Board
open GameLogic

let randomNum = System.Random()

let rec shuffleBoardOnce board size level =

    if level = 0 then board
    else
        let movableTiles = possibleTiles board size
        let chosenTile = movableTiles.[randomNum.Next(movableTiles.Length)]
        let updatedBoard = moveTile board size chosenTile
        
        shuffleBoardOnce updatedBoard size (level - 1)

let rec shuffleBoard size =

    let solved = createSolved size
    let level = randomNum.Next(30, 50)
    let boardCreated = shuffleBoardOnce solved size level 

    if boardCreated = solved then shuffleBoard size
    else boardCreated