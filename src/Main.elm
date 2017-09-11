module Main exposing (..)

import DeckGenerator
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import Process
import Random
import Task
import Time exposing (Time)
import Date exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init =
            ( { game = Choosing DeckGenerator.static
              , score = 0
              , name = ""
              , t0 = 0
              , t1 = 0
              , highscores = []
              }
            , generateDeck
            )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


generateDeck : Cmd Msg
generateDeck =
    Random.generate StartGame DeckGenerator.random


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none


delay : Time.Time -> Msg -> Cmd Msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


updateScore : GameState -> Int -> Int
updateScore game score =
    case game of
        Matching _ _ ->
            score + 1

        _ ->
            score


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EndGame ->
            ( model, Task.perform EndTime Time.now )

        NewTime time ->
            ( { model | t0 = time }, Cmd.none )

        EndTime time ->
            ( { model | t1 = time, game = GameOver }, Cmd.none )

        CardClick card ->
            ( { model
                | game = updateCardClick card model.game
                , score = updateScore model.game model.score
              }
            , Cmd.none
            )

        RestartGame ->
            ( { model
                | game = Choosing DeckGenerator.static
                , score = 0
              }
            , generateDeck
            )

        Cheat ->
            ( { model | game = cheatMode model.game }
            , delay
                (Time.second * 1)
                EndGame
            )

        StartGame deck ->
            ( { model | game = Choosing deck }, Task.perform NewTime Time.now )

        NameInput name ->
            ( { model | name = name }, Cmd.none )

        SaveName ->
            ( { model
                | game = HighScore
                , name = ""
                , highscores =
                    saveHighscore model
              }
            , Cmd.none
            )


saveHighscore : Model -> List UserScore
saveHighscore model =
    { name = model.name, score = model.score } :: model.highscores


cheatMode : GameState -> GameState
cheatMode state =
    case state of
        Choosing deck ->
            deck |> setAllToMatched |> Choosing

        Matching card deck ->
            deck |> setAllToMatched |> Choosing

        _ ->
            state


setAllToMatched : Deck -> Deck
setAllToMatched deck =
    List.map (\c -> { c | state = Matched }) deck


zeroPad : String -> String
zeroPad timeString =
    if String.length timeString == 1 then
        "0"
            ++ timeString
    else
        timeString


printTime : Time -> Html Msg
printTime time =
    let
        seconds =
            Time.inSeconds time |> round |> toString |> zeroPad

        minutes =
            Time.inMinutes time |> round |> toString |> zeroPad

        hours =
            Time.inHours time |> round |> toString |> zeroPad
    in
        text <| "Your time is: " ++ hours ++ ":" ++ minutes ++ ":" ++ seconds ++ "s"


view : Model -> Html Msg
view model =
    case model.game of
        Choosing deck ->
            viewBoard model deck

        Matching card deck ->
            viewBoard model deck

        GameOver ->
            div [ class "victory" ]
                [ div []
                    [ text "You won!"
                    , div []
                        [ text
                            ("Your score is "
                                ++ (toString
                                        model.score
                                   )
                            )
                        ]
                    , printTime (model.t1 - model.t0)
                    , enterNameForm
                    , newGameButton
                    ]
                ]

        HighScore ->
            div []
                [ h3 [] [ text "HighScore" ]
                , ul [] (viewHighScores model.highscores)
                , newGameButton
                ]


newGameButton : Html Msg
newGameButton =
    button
        [ onClick RestartGame
        , class "btn"
        ]
        [ text "New game" ]


viewHighScores : List UserScore -> List (Html Msg)
viewHighScores highscores =
    highscores
        |> List.sortBy .score
        |> List.reverse
        |> List.map viewHighScore


viewHighScore : UserScore -> Html Msg
viewHighScore userscore =
    li []
        [ text
            (userscore.name
                ++ "     "
                ++ (toString userscore.score)
            )
        ]


enterNameForm : Html Msg
enterNameForm =
    Html.form
        [ onSubmit SaveName ]
        [ label
            [ for "entername" ]
            [ text "Enter name" ]
        , input
            [ id "entername"
            , onInput NameInput
            ]
            []
        , button
            [ class "btn", type_ "submit" ]
            [ text "Save" ]
        ]


closeUnmatched : Deck -> Deck
closeUnmatched deck =
    List.map
        (\c ->
            if c.state /= Matched then
                { c | state = Closed }
            else
                c
        )
        deck


setCard : Card -> CardState -> Deck -> Deck
setCard card newState deck =
    List.map
        (\c ->
            if c.id == card.id && c.group == card.group then
                { c | state = newState }
            else
                c
        )
        deck


isMatching : Card -> Card -> Bool
isMatching card1 card2 =
    card1.group /= card2.group && card1.id == card2.id


allMatching : Deck -> Bool
allMatching deck =
    List.all (\c -> c.state == Matched) deck


updateCardClick : Card -> GameState -> GameState
updateCardClick clickedCard state =
    case state of
        Choosing deck ->
            let
                updatedDeck =
                    deck
                        |> closeUnmatched
                        |> setCard clickedCard Open
            in
                Matching clickedCard updatedDeck

        Matching openCard deck ->
            let
                updatedDeck =
                    if isMatching clickedCard openCard then
                        deck
                            |> setCard openCard Matched
                            |> setCard clickedCard Matched
                    else
                        setCard clickedCard Open deck
            in
                if allMatching updatedDeck then
                    GameOver
                else
                    Choosing updatedDeck

        _ ->
            state


viewBoard : Model -> Deck -> Html Msg
viewBoard model cards =
    div []
        [ h3 []
            [ text "Memory Meow" ]
        , div
            [ class "cards" ]
            (List.map viewCard cards)
        , div []
            [ text ("Score: " ++ (toString model.score))
            , button
                [ onClick RestartGame
                , class "btn"
                ]
                [ text "Restart" ]
            , button
                [ onClick Cheat
                , class "btn"
                ]
                [ text "Cheat" ]
            ]
        ]


viewCard : Card -> Html Msg
viewCard card =
    case card.state of
        Open ->
            cardImg ("/static/cats/" ++ card.id ++ ".jpg") "open"

        Matched ->
            cardImg ("/static/cats/" ++ card.id ++ ".jpg") "matched"

        Closed ->
            img
                [ src "/static/cats/closed.png"
                , class "closed card"
                , onClick (CardClick card)
                ]
                []


cardImg : String -> String -> Html a
cardImg imgPath cls =
    img
        [ src imgPath
        , class (cls ++ " card")
        ]
        []
