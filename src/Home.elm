module Home exposing (Model, Msg, init, update, view)

import Html exposing (Html, h3, div, text)
import Http
import Json.Decode as D

type alias Model =
    { baseUrl: String
    , description: String
    }


type Msg
    = HomeMsg
    | GotGreetings (Result Http.Error String)


init : String -> (Model, Cmd Msg)
init baseUrl =
    ({ baseUrl = baseUrl
     , description = "welcome home!~"
     }
    , loadGreeting baseUrl)


loadGreeting : String -> Cmd Msg
loadGreeting baseUrl =
    Http.get { url = String.concat [baseUrl, "/api/hello.json"]
             , expect = Http.expectJson GotGreetings (D.field "greeting" D.string)}


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        HomeMsg ->
            (model, Cmd.none)
        GotGreetings res ->
            case res of
                Ok result ->
                    ({ model | description = result }, Cmd.none)
                Err _ ->
                    ({ model | description = "Http request failed!"}, Cmd.none)


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text model.description ]]

