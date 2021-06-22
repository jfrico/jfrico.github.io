module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (Html, div, text, h1, a, li, ul, header)
import Html.Attributes exposing (href, id)
import Html.Lazy exposing (lazy2)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s, string)
import Json.Encode as E
import Json.Decode as D exposing(Decoder)

import Home
import About

type Page
    = HomePage Home.Model
    | AboutPage About.Model
    | NotFound

type Route
    = HomeRoute
    | AboutRoute

type alias Model =
    { page: Page
    , key: Nav.Key
    , baseUrl: String
    }

type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
    | GotHomeMsg Home.Msg
    | GotAboutMsg About.Msg

toHome : Model -> (Home.Model, Cmd Home.Msg) -> (Model, Cmd Msg)
toHome model (home, msg) =
    ( { model | page = HomePage home}
    , Cmd.map GotHomeMsg msg)

toAbout: Model -> (About.Model, Cmd About.Msg) -> (Model, Cmd Msg)
toAbout model (about, msg) =
    ({ model | page = AboutPage about}
    , Cmd.map GotAboutMsg msg)

parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map HomeRoute Parser.top
        , Parser.map AboutRoute (s "about")
        ]


updateUrl : Url -> Model -> (Model, Cmd Msg)
updateUrl url model =
    case Parser.parse parser url of
        Just HomeRoute ->
            Home.init model.baseUrl |> toHome model
        Just AboutRoute ->
            About.init model.baseUrl |> toAbout model
        Nothing ->
            ({ model | page = NotFound }, Cmd.none)


init : String -> Url -> Nav.Key -> (Model, Cmd Msg)
init baseUrl url key =
    updateUrl url { page = NotFound
                  , key = key
                  , baseUrl = baseUrl
                  }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.External href ->
                    (model, Nav.load href)
                Browser.Internal url ->
                    (model, Nav.pushUrl model.key (Url.toString url))

        UrlChanged url ->
            updateUrl url model

        GotHomeMsg homeMsg ->
            case model.page of
                HomePage str ->
                    toHome model (Home.update homeMsg str)
                _ ->
                    (model, Cmd.none)

        GotAboutMsg aboutMsg ->
            case model.page of
                AboutPage str ->
                    toAbout model (About.update aboutMsg str)
                _ ->
                    (model, Cmd.none)


viewLink : String -> String -> Html Msg
viewLink path name =
    li [] [ a [ href path ] [ text name ] ]


viewHeader : Html Msg
viewHeader =
    header [] [ ul [] [ lazy2 viewLink "/" "Home"
                      , lazy2 viewLink "/about" "About"
                      ]]


view : Model -> Document Msg
view model =
    let content = case model.page of
                    HomePage home ->
                        Home.view home |> Html.map GotHomeMsg
                    AboutPage about ->
                        About.view about |> Html.map GotAboutMsg
                    NotFound ->
                        text "404 Not Found!"
    in
        { title = "jfrico"
        , body = [ div [ id "app" ]
                       [ viewHeader
                       , div [] [ content ]]]}


main : Program String Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }

