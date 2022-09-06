module App

open Elmish
open Elmish.React
open Feliz
open Fable.Core

type Running =
    {
        Score : int
        Numbers : int list
        Clicked : int list
        TickId : int
    }

type State =
    | NotStarted
    | Running of Running
    | Finished of int

type Msg =
    | Clicked of int
    | NewNumber of int
    | Start
    | Restart
    | GameStarted of int

module Extensions =
    [<Emit("setInterval($0, $1)")>]
    let setInterval (f: unit -> unit) (n: int) : int = jsNative

    [<Emit("clearInterval($0)")>]
    let clearInterval (n: int) : unit = jsNative


module State =
    open Extensions

    let private withoutCommands state =
        state, Cmd.none

    let random = System.Random()

    let private removeIndexAt index list =
        list
        |> List.mapi (fun i e -> (i <> index , e))
        |> List.filter fst
        |> List.map snd


    let startGame =
        let start dispatch =
            let callback = (fun () -> dispatch <| NewNumber (random.Next(1, 9)))
            let tickId = setInterval callback 1100
            dispatch (GameStarted tickId)

        Cmd.ofSub start

    let stopTicking tickId =
        let stopTicking =
            fun _ ->
                clearInterval tickId
                ()

        Cmd.ofSub stopTicking


    let init() =
        NotStarted, Cmd.none


    let private initializedGame tickId =
        {
            Score = 0
            Numbers = []
            Clicked = []
            TickId = tickId
        }

    let private finishedGame state =
        Finished state.Score, stopTicking state.TickId

    let private (|AddedExactlyTen|ClickedTooMany|StillGood|) state =
        if state.Clicked |> List.sum = 10 then
            AddedExactlyTen
        elif state.Clicked |> List.length >= 3 then
            ClickedTooMany
        else
            StillGood

    let private handleClickOn index state number =
        let newState =
            { state with
                Clicked = number :: state.Clicked
                Numbers = state.Numbers |> removeIndexAt index
            }

        match newState with
        | AddedExactlyTen ->
            Running {
                newState with
                    Score = newState.Score + 1
                    Clicked = []
            } |> withoutCommands
        | ClickedTooMany ->
            finishedGame newState
        | StillGood ->
            Running newState |> withoutCommands

    let private withHandledClickOn index state =
        state.Numbers
        |> List.tryItem index
        |> Option.map (handleClickOn index state)
        |> Option.defaultValue (Running state |> withoutCommands)

    let private withAddedNumber number state =
        let newState = {
            state with Numbers = state.Numbers @ [number]
        }

        if newState.Numbers |> List.length = 10 then
            finishedGame newState
        else
            Running newState |> withoutCommands


    let update (msg: Msg) (state: State) =
        match state, msg with

        | NotStarted, Start ->
            state, startGame

        | Finished _, Restart ->
            state, startGame

        | (NotStarted | Finished _), GameStarted tickId ->
             initializedGame tickId
             |> Running
             |> withoutCommands

        | Running state, Clicked index ->
            state |> withHandledClickOn index

        | Running state, NewNumber number ->
            state |> withAddedNumber number

        | _ -> state, Cmd.none

module View =
    let private renderNotStarted dispatch =
        Html.div [
            (*
            Tens!

Numbers are going to show up on the screen.

Select two or three numbers that equal 10 when added up.

If they are over ten, GAME OVER!

If you choose three and they are under ten, GAME OVER!

If 10 numbers are displayed without clearing any out, GAME OVER!
            *)
            Html.button [
                prop.style [
                    style.fontSize 20
                    style.padding 20
                ]
                prop.onClick (fun _ -> dispatch Start)
                prop.text "Start Game"
            ]
        ]

    let private renderNumberButton dispatch index (number : int) =
        Html.button [
            prop.style [
                style.fontSize 20
                style.padding 20
            ]
            prop.onClick (fun _ -> dispatch <| Clicked index)
            prop.text number
        ]


    let private renderRunning state dispatch =
        Html.div [
            yield! state.Numbers |> List.mapi (renderNumberButton dispatch)
        ]

    let private renderFinished score dispatch =
        Html.div [
            Html.div (sprintf "Score: %i" score)
            Html.button [
                prop.style [
                    style.fontSize 20
                    style.padding 20
                ]
                prop.onClick (fun _ -> dispatch Restart)
                prop.text "Restart"
        ]

        ]

    let render (state: State) (dispatch: Msg -> unit) =
      match state with
      | NotStarted ->
          renderNotStarted dispatch

      | Running state ->
          renderRunning state dispatch

      | Finished score ->
          renderFinished score dispatch


Program.mkProgram State.init State.update View.render
|> Program.withReactSynchronous "elmish-app"
|> Program.run
