module Context
    exposing
        ( Flags
        , Model
        , Msg(..)
        , update
        , init
        )

import Application exposing (Transition(..))
import Url exposing (Url)


type alias User =
    { name : String
    , email : String
    }


type alias Flags =
    ()


type alias Model =
    { url : Url
    , transition : Application.Transition
    , user : Maybe User
    }


type Msg
    = SetUser User
    | RemoveUser


init : Flags -> Url -> ( Model, Cmd Msg )
init flags url =
    ( Model
        url
        NotReady
        (Just <| User "Ryan" "ryan.nhg@gmail.com")
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetUser user ->
            ( { model | user = Just user }
            , Cmd.none
            )

        RemoveUser ->
            ( { model | user = Nothing }
            , Cmd.none
            )
