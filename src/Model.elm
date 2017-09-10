module Model exposing (..)


type alias UserScore =
    { name : String
    , score : Int
    }


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
    | HighScore


type alias Model =
    { game : GameState
    , score : Int
    , name : String
    , highscores : List UserScore
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
    | NameInput String
    | SaveName


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
