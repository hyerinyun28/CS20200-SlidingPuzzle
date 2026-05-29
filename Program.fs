open Board
open GameLogic
open Shuffle

let updateScreen () = System.Console.Clear()

/// UX design - divided into two parts: Start and Board page
let printBoardScreen board size moves command =
    updateScreen()
    printfn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    printfn "           CLI Sliding Puzzle           "
    printfn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    printfn ""

    printBoard board size

    printfn ""
    printfn "Current Moves ------------   %d" moves
    printfn ""
    printfn "Enter tile number, or q to quit"

    match command with
    | Some cmd -> 
        printfn ""
        printfn "%s" cmd
    | None -> ()

let printStartScreen cmd =
    updateScreen()
    printfn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    printfn "           CLI Sliding Puzzle           "
    printfn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    printfn ""
    printfn "     Welcome to CLI Sliding Puzzle !    "
    printfn ""
    printfn "Select Sliding Puzzle Size: "
    printfn ""
    printfn "    [1]    3  x  3   Beginner"
    printfn "    [2]    4  x  4   Challenge"
    printfn ""

    match cmd with
    | Some cmd -> 
        printfn "%s" cmd
        printfn ""
    | None -> ()

    printf "Enter your choice: "

let rec puzzleSize cmd =
    printStartScreen cmd
    let input = System.Console.ReadLine()

    match input with 
    | "1" -> 3
    | "2" -> 4
    | _ ->
        puzzleSize (Some "Please enter either 1 or 2 only.")


/// Repeat the puzzle until the user succeeds
let rec loop board size moves (startTime: System.DateTime) cmd =

    printBoardScreen board size moves cmd

    if isSolved board size then

        let endTime = System.DateTime.Now
        let timeSpent = endTime - startTime

        printfn "Puzzle solved!"
        printfn "Total moves: %d" moves
        printfn "Total Time Spent: %.2f seconds" timeSpent.TotalSeconds

    else

        let input = System.Console.ReadLine()

        if input = "q" then printfn "Game terminated."

        else 

            match System.Int32.TryParse(input) with
            | true, tile ->
                if possibleMove board size tile then
                    let newBoard = moveTile board size tile
                    loop newBoard size (moves + 1) startTime None
                else 
                    loop board size moves startTime (Some "The chosen tile is not adjacent to an empty tile.")
            | false, _ ->
                loop board size moves startTime (Some "Please choose the valid input.")

[<EntryPoint>]
let main argv =
    let size = puzzleSize None
    let board = shuffleBoard size
    let startTime = System.DateTime.Now
    loop board size 0 startTime None

    0