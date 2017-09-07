module Main exposing (..)

import DeckGenerator
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Model exposing (..)
import Process
import Random
import Task
import Time


main =
    Html.program
        { init =
            ( { game = Choosing DeckGenerator.static }
            , generateDeck
            )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


generateDeck : Cmd Msg
generateDeck =
    Random.generate RandomDeck DeckGenerator.random


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none


delay : Time.Time -> Msg -> Cmd Msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CardClick card ->
            ( { model | game = updateCardClick card model.game }, Cmd.none )

        RestartGame ->
            ( { model | game = Choosing DeckGenerator.static }, generateDeck )

        Cheat ->
            ( { model | game = cheatMode model.game }, delay (Time.second * 2) GoToGameover )

        RandomDeck deck ->
            ( { model | game = Choosing deck }, Cmd.none )

        GoToGameover ->
            ( { model | game = GameOver }, Cmd.none )


cheatMode : GameState -> GameState
cheatMode state =
    case state of
        Choosing deck ->
            deck |> setAllToMatched |> Choosing

        Matching card deck ->
            deck |> setAllToMatched |> Choosing

        GameOver ->
            GameOver


setAllToMatched : Deck -> Deck
setAllToMatched deck =
    List.map (\c -> { c | state = Matched }) deck


view : Model -> Html Msg
view model =
    case model.game of
        Choosing deck ->
            viewCards deck

        Matching card deck ->
            viewCards deck

        GameOver ->
            div [ class "victory" ]
                [ div []
                    [ text "You won!"
                    , button
                        [ onClick RestartGame
                        , class "btn"
                        ]
                        [ text "New game" ]
                    ]
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

        GameOver ->
            GameOver


viewCards : List Card -> Html Msg
viewCards cards =
    div []
        [ h3 []
            [ text "Memory Meow" ]
        , div
            [ class "cards" ]
            (List.map viewCard cards)
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
