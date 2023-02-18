module Main exposing (..)

import Browser
import Form.Field as Field
import Form.Html as Form exposing (Form)
import Form.Internals as Form
import Form.SimpleFields as Field
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Url
import Url.Builder as Url


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { form : Form Input Input (List String)
    , gifSearch : List String
    }


type alias Input =
    { message : String
    , gif : GifValue
    }


type Msg
    = FormMsg (Form Input Input (List String))
    | GotGifs (Result Http.Error (List String))
    | Submit


form : Form Input Input (List String)
form =
    let
        message =
            Field.textField
                { textType = Field.Text }
                ""
                |> Field.withPlaceholder "Message"

        gif =
            gifField
                { searchField = ""
                , chosen = Nothing
                }
                |> Field.withHints
                    [ .chosen >> Field.isDefined "Please choose a gif" ]
    in
    Form.succeed Input Input
        |> Form.append message
        |> Form.append gif


getGifs : String -> Cmd Msg
getGifs search =
    let
        decoder =
            D.field "gfycats"
                (D.list
                    (D.field "content_urls"
                        (D.field "max1mbGif" (D.field "url" D.string))
                    )
                )
    in
    Http.get
        { url =
            Url.crossOrigin "https://api.gfycat.com"
                [ "v1", "gfycats", "search" ]
                [ Url.string "search_text" search
                , Url.int "count" 8
                , Url.int "start" 0
                ]
        , expect = Http.expectJson GotGifs decoder
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { form = form
      , gifSearch = []
      }
    , getGifs "programmer"
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormMsg fmsg ->
            let
                form1 =
                    Form.get fmsg

                form2 =
                    Form.get model.form
            in
            ( { model | form = fmsg }
            , if form1.gif.searchField /= form2.gif.searchField then
                getGifs form1.gif.searchField

              else
                Cmd.none
            )

        Submit ->
            case Form.isValid model.gifSearch model.form of
                Ok _ ->
                    ( model, Cmd.none )

                Err f ->
                    ( { model | form = f }
                    , Cmd.none
                    )

        GotGifs (Ok res) ->
            ( { model | gifSearch = res }
            , Cmd.none
            )

        GotGifs (Err e) ->
            ( model
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "padding" "4em"
        ]
        [ Html.form
            [ onSubmit Submit
            , class "form"
            , style "max-width" "40rem"
            ]
            [ div []
                (Form.view
                    model.gifSearch
                    model.form
                )
                |> Html.map FormMsg
            , button
                [ type_ "submit" ]
                [ text "Submit" ]
            ]
        ]



-------------------------------------------------------------------------------
--- Start of GifField
-------------------------------------------------------------------------------


type alias GifValue =
    { searchField : String
    , chosen : Maybe String
    }


type alias GifContext =
    List String


type alias GifField form ctx =
    Field.Field form GifValue {} GifMsg String ctx ctx (Html (Field.FieldMsg GifMsg))


type alias GifFieldState =
    Field.Value GifValue {}


gifField : GifValue -> GifField form GifContext
gifField value =
    { view = gifFieldView
    , state =
        { value = value
        }
    , update = gifFieldUpdate
    , hints = []
    , label = Nothing
    , placeholder = Nothing
    , description = Nothing
    , getContext = identity
    , hinting = []
    , globalHinting = []
    }


type GifMsg
    = SearchFor String
    | Choose String


gifFieldView : GifContext -> Field.ViewConfig GifValue {} String -> Html (Field.FieldMsg GifMsg)
gifFieldView ctx field =
    let
        search =
            input
                [ class "form-input"
                , onInput SearchFor
                , type_ "search"
                , placeholder "Search for a gif"
                , value field.state.value.searchField
                ]
                []
                |> Html.map Field.FieldMsg

        listOfGifs =
            div
                [ style "display" "flex"
                , style "gap" "12px"
                , style "flex-wrap" "wrap"
                ]
                (List.map
                    (\gif ->
                        img
                            [ src gif
                            , style "align-self" "center"
                            , style "width" "8rem"
                            , style "cursor" "pointer"
                            , case field.state.value.chosen of
                                Just v ->
                                    if v == gif then
                                        style "opacity" "0.5"

                                    else
                                        class ""

                                Nothing ->
                                    class ""
                            , onClick (Field.FieldMsg <| Choose gif)
                            ]
                            []
                    )
                    ctx
                )

        selectedGif =
            case field.state.value.chosen of
                Just v ->
                    img
                        [ src v
                        , style "width" "35rem"
                        ]
                        []

                Nothing ->
                    text ""
    in
    fieldset []
        [ search
        , listOfGifs
        , selectedGif
        , div []
            (List.map
                (\e ->
                    div [ class "form-hint" ] [ text e ]
                )
                field.hints
            )
        ]


gifFieldUpdate : ctx -> GifMsg -> Field.Value GifValue {} -> Field.Value GifValue {}
gifFieldUpdate _ msg state =
    let
        value =
            state.value
    in
    case msg of
        SearchFor search ->
            { state
                | value = { value | searchField = search }
            }

        Choose search ->
            { state | value = { value | chosen = Just search } }



-------------------------------------------------------------------------------
--- End of GifField
-------------------------------------------------------------------------------
