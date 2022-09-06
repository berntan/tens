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

    let random = System.Random()

    let removeIndexAt index list =
        list
        |> List.mapi (fun i e -> (i <> index , e))
        |> List.filter fst
        |> List.map snd


    let startGame =
        let start dispatch =
            let callback = (fun () -> dispatch <| NewNumber (random.Next(1, 9)))
            let tickId = setInterval callback 1500
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

    let update (msg: Msg) (state: State) =
        match state, msg with
        | NotStarted, Start ->
            state, startGame

        | Finished _, Restart ->
            state, startGame

        | (NotStarted | Finished _), GameStarted tickId ->
            Running (initializedGame tickId), Cmd.none

        | Running state, Clicked index ->
            state.Numbers
            |> List.tryItem index
            |> Option.map (fun number ->
                let newState =
                    { state with
                        Clicked = number :: state.Clicked
                        Numbers = state.Numbers |> removeIndexAt index
                    }
                if newState.Clicked |> List.sum = 10 then
                    Running {
                        newState with
                            Score = newState.Score + 1
                            Clicked = []
                    }, Cmd.none
                elif newState.Clicked |> List.length >= 3 then
                    Finished newState.Score, stopTicking newState.TickId
                else
                    Running newState, Cmd.none
            )
            |> Option.defaultValue (Running state, Cmd.none)

        | Running state, NewNumber number ->
            let newState = {
                state with Numbers = state.Numbers @ [number]
            }

            if newState.Numbers |> List.length = 10 then
                Finished newState.Score, stopTicking newState.TickId
            else
                Running newState, Cmd.none

        | _ -> state, Cmd.none

module View =
    let private renderNotStarted dispatch =
        Html.div [
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
