module Model exposing (..)


type CardState
    = Open
    | Closed
    | Matched


type alias Card =
    { id : String
    , state : CardState
    , group : Group
    }


type GameState
    = Choosing Deck
    | Matching Card Deck
    | GameOver

type alias Model =
    { game : GameState
    , score : Int
    }


type Group
    = A
    | B


type alias Deck =
    List Card


type Msg
    = CardClick Card
    | RestartGame
    | Cheat
    | RandomDeck Deck
    | GoToGameover


openCard : Card
openCard =
    { id = "1"
    , state = Open
    , group = A
    }


closedCard : Card
closedCard =
    { id = "1"
    , state = Closed
    , group = A
    }


matchedCard : Card
matchedCard =
    { id = "1"
    , state = Matched
    , group = A
    }
