module GameLogic

/// Logic functions that needed to enable the game flow

/// finding the tile index of the number chosen by the user
let index board tile = 
    board
    |> List.findIndex (fun x -> x = tile)

/// check whether the chosen tile can be swapped with an empty tile
let possibleMove board size tile =

    if tile = 0 || not (List.contains tile board) then false
    else
    
        let tileIndex = index board tile
        let emptyIndex = index board 0

        let tileRow = tileIndex / size
        let tileCol = tileIndex % size

        let emptyRow = emptyIndex / size
        let emptyCol = emptyIndex % size

        let rowDiff = abs (tileRow - emptyRow)
        let colDiff = abs (tileCol - emptyCol)

        rowDiff + colDiff = 1

let swap board indexi indexj =

    let i = List.item indexi board
    let j = List.item indexj board

    board
    |> List.mapi (fun index value ->

        if index = indexi then j
        elif index = indexj then i
        else value
    )

let moveTile board size tile = 

    if possibleMove board size tile then

        let tileIndex = index board tile
        let emptyIndex = index board 0

        swap board tileIndex emptyIndex

    else board

/// List of tiles that is valid for swapping at a current state
let possibleTiles board size =
    board
    |> List.filter (fun tile ->
        tile <> 0 && possibleMove board size tile
    )
