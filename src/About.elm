module About exposing (Model, Msg, init, update, view)

import Html exposing (Html, h3, div, text)
import Http
import Json.Decode as D


type alias Model =
    { baseUrl: String
    , about: String
    , description: String
    }


type Msg
    = AboutMsg
    | GotDescription (Result Http.Error String)


init: String -> (Model, Cmd Msg)
init baseUrl =
    ({ baseUrl = baseUrl
     , about = "About"
     , description = "About Page"
     }
    , loadDescription baseUrl)


loadDescription: String -> Cmd Msg
loadDescription baseUrl =
    Http.get { url = String.concat [baseUrl, "/api/about.json"]
             , expect = Http.expectJson GotDescription (D.field "description" D.string)}


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AboutMsg ->
            (model, Cmd.none)
        GotDescription response ->
            case response of
                Ok result ->
                    ({ model | description = result }, Cmd.none)
                Err _ ->
                    ({ model | description = "Http request failed!"}, Cmd.none)


view: Model -> Html Msg
view model =
    div []
        [ div [] [ text model.description ]]
