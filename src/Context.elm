module Context exposing (..)


type alias User =
    { name : String
    , email : String
    }


type alias Flags =
    ()


type alias Model =
    { user : Maybe User
    }


type Msg
    = SetUser User
    | RemoveUser
