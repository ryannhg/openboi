module Context
    exposing
        ( Flags
        , Model
        , Msg(..)
        , update
        , init
        )


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


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model
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
