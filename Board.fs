module Board

/// Board-related functions

/// Basic form (Target goal - solved version) of the board
let createSolved size = [1 .. (size * size - 1)] @ [0]

let printTile tile = 
    if tile = 0 then printf "[    ]"
    else printf "[ %2d ]" tile

let printBoard board size =
    board
    |> List.iteri (fun i tile ->
        printTile tile
        if (i + 1) % size = 0 then printfn "" // 줄바꿈
    )

/// check whether the user has succeeded, or solved the puzzle.
let isSolved board size =
    board = createSolved size