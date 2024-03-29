module App

(*

https://en.wikipedia.org/wiki/Conway's_Game_of_Life

The universe of the Game of Life is an infinite, two-dimensional orthogonal grid of square cells,
each of which is in one of two possible states, live or dead, (or populated and unpopulated, respectively).
Every cell interacts with its eight neighbours, which are the cells that are horizontally, vertically, or
diagonally adjacent. At each step in time, the following transitions occur:

    Any live cell with fewer than two live neighbours dies, as if by underpopulation.
    Any live cell with two or three live neighbours lives on to the next generation.
    Any live cell with more than three live neighbours dies, as if by overpopulation.
    Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

These rules, which compare the behavior of the automaton to real life, can be condensed into the following:

    Any live cell with two or three live neighbours survives.
    Any dead cell with three live neighbours becomes a live cell.
    All other live cells die in the next generation. Similarly, all other dead cells stay dead.

The initial pattern constitutes the seed of the system. The first generation is created by applying the above
rules simultaneously to every cell in the seed, live or dead; births and deaths occur simultaneously, and the
discrete moment at which this happens is sometimes called a tick.[nb 1] Each generation is a pure function of
the preceding one. The rules continue to be applied repeatedly to create further generations.

*)

open Browser.Dom
open Browser.Types

open Fable.Core
open Fable.Core.JsInterop

type GameState = {
    // What round of the game it is.
    round: int;
    
    // The cells which are alive
    cells: Set<(int * int)>;

    // How big the board is.
    width: int;
    height: int;
}

module GameState =
    // List of vectors to find neibors of a given point
    let neighbor_pos_vec = [|
        (-1, 1); (0, 1); (1, 1);
        (-1, 0); (1, 0);
        (-1, -1); (0, -1); (1, -1);
    |]

    // Actions which can be applied to a GameState to give the next GameState.
    type Action =
        | Clear
        | FillRandom of probability: float * rnd: (unit -> float)
        | Step

    // Creates an initial empty game state.
    let create (width: int) (height: int): GameState = 
        {
            round = 0;
            cells = Set.empty;
            width = width;
            height = height;
        }

    // Determines the count of live neighbors.
    let get_cell_live_neighbor_count (gs: GameState) (x, y) =
        neighbor_pos_vec
        |> Seq.map (fun (dx, dy) -> (x + dx, y + dy))
        |> Seq.filter (fun p -> Set.contains p gs.cells)
        |> Seq.length

    // Get all of the cell positions which have the potential to change in the next step.
    let get_cells_to_evaluate (gs: GameState) =
        // Get the live cells and any adjacent cells to live ones.
        let neighbors =
            gs.cells
            |> Seq.collect (fun (x, y) ->
                neighbor_pos_vec |> Seq.map (fun (dx, dy) -> (x + dx, y + dy))
            )

        Seq.append neighbors gs.cells |> Set.ofSeq

    // Should a cell be alive in the next step?
    let cell_should_live (x, y) (gs: GameState) =
        let l_count = get_cell_live_neighbor_count gs (x, y)
        let is_alive = Set.contains (x, y) gs.cells

        let will_live =
            if is_alive then
                if l_count < 2 then
                    false
                elif l_count >= 2 && l_count <= 3 then
                    true
                else
                    false
            else if l_count = 3 then
                true
            else
                false

        will_live

    // Sets a cell to live
    let add_cell x y (gs: GameState) =
        if x < 0 || x >= gs.width || y < 0 || y >= gs.height then
            gs
        else
            { gs with
                cells = Set.add (x, y) gs.cells
            }
        
    // Advance the game state 1 step
    let advance (gs: GameState) =
        let apply_cell_action (cells: Set<int * int>) (should_live, (x, y)) =
            if should_live then
                Set.add (x, y) cells
            else
                Set.remove (x, y) cells
        
        let next_cells =
            gs
            |> get_cells_to_evaluate
            |> Seq.map (fun p -> (cell_should_live p gs, p))
            |> Seq.fold apply_cell_action gs.cells

        { gs with cells = next_cells }

    // Fill out some cells randomly.
    let fill_random (p: float) (rand: unit -> float) (gs: GameState): GameState =
        let points =
            { 0 .. gs.width } |> Seq.collect (fun x ->
                { 0 .. gs.height } |> Seq.map (fun y -> (x, y))
            )

        let new_state =
            points
            |> Seq.filter (fun point -> rand() <= p)
            |> Seq.fold (fun gs (x, y) -> add_cell x y gs) gs

        new_state

    // Applies an action to a game state and returns the new state.
    let apply (action: Action) (gs: GameState): GameState =
        match action with
            | Clear ->
                { gs with cells = Set.empty; round = 0; }
            | FillRandom (p, rnd) ->
                fill_random p rnd gs
            | Step ->
                advance gs

// Responsible for rendering the game state to an HTML canvas.
module GOFRenderer =

    let render_bg (canvas: HTMLCanvasElement) (gs: GameState) =
        let ctx = canvas.getContext_2d()

        ctx.rect(0.0, 0.0, float canvas.width, float canvas.height)
        ctx.fillStyle <- U3.Case1 "black"
        ctx.fill()

    let render_cell (canvas: HTMLCanvasElement) (gs: GameState) (x: int) (y: int) =
        let x_step = canvas.width / float (gs.width + 1)
        let y_step = canvas.height / float (gs.height + 1)

        let x_start = float x * x_step
        let y_start = float y * y_step

        let ctx = canvas.getContext_2d()
        
        ctx.fillStyle <- U3.Case1 "green"
        ctx.fillRect(x_start, y_start, x_step, y_step)

    let render (canvas: HTMLCanvasElement) (gs: GameState) =
        render_bg canvas gs
        for (x, y) in gs.cells do
            render_cell canvas gs x y

// Runs the game.
module GameOfLife =

    let initialize () =
        let mutable gs = GameState.create 100 100
        let canvas = document.getElementById("gof_canvas") :?> HTMLCanvasElement
        let rnd = JS.Math.random
        let p = 0.5

        let action_processor (action: GameState.Action) =
            gs <- GameState.apply action gs
            GOFRenderer.render canvas gs

        action_processor GameState.Action.Clear
        action_processor (GameState.Action.FillRandom(p, rnd))
        
        // Button clicks clear and restart.
        let reset_btn = document.getElementById("btn_reset") :?> HTMLButtonElement
        reset_btn.onclick <- (fun e ->
            action_processor GameState.Action.Clear
            action_processor (GameState.Action.FillRandom(p, rnd))
        )

        // Run 1 step on timer
        let timer_id = JS.setInterval (fun () -> action_processor GameState.Action.Step) 100

        ()

GameOfLife.initialize()