module Main exposing (..)

import DeckGenerator
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Model exposing (..)


main =
    Html.beginnerProgram
        { model = { game = Choosing DeckGenerator.static }
        , view = view
        , update = update
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        CardClick card ->
            { model | game = updateCardClick card model.game }


view : Model -> Html Msg
view model =
    case model.game of
        Choosing deck ->
            viewCards deck

        Matching card deck ->
            viewCards deck

        GameOver ->
            div [] [ text "You won!" ]


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
                        deck
            in
                Matching openCard deck

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
                , class "closed"
                , onClick (CardClick card)
                ]
                []


cardImg : String -> String -> Html a
cardImg imgPath cls =
    div []
        [ img
            [ src imgPath
            , class cls
            ]
            []
        ]
