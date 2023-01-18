module Form.SimpleFields exposing (..)

import Form.Field as Form exposing (Field, FieldMsg(..), Value, ViewConfig)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type TextType
    = Text
    | TextArea
    | Password
    | Email
    | Search


type alias TextFieldConfig =
    { textType : TextType
    }


type alias TextField form ctx =
    Field form String TextFieldConfig String String ctx ctx (Html (FieldMsg String))


type TextFieldState
    = Value String TextFieldConfig


textField : TextFieldConfig -> String -> TextField form ctx
textField config value =
    { view = textFieldView
    , state =
        { value = value
        , textType = config.textType
        }
    , update = textFieldUpdate
    , hints = []
    , label = Nothing
    , placeholder = Nothing
    , description = Nothing
    , getContext = identity
    , hinting = []
    , globalHinting = []
    }


textFieldView : ctx -> ViewConfig String TextFieldConfig String -> Html (FieldMsg String)
textFieldView _ field =
    let
        textType =
            case field.state.textType of
                Text ->
                    "text"

                TextArea ->
                    "text"

                Password ->
                    "password"

                Email ->
                    "email"

                Search ->
                    "search"

        element =
            case field.state.textType of
                TextArea ->
                    textarea

                _ ->
                    input
    in
    div []
        [ label [ class "form-label" ]
            [ text (Maybe.withDefault "" field.label)
            , element
                [ type_ textType
                , class "form-input"
                , case field.hints of
                    [] ->
                        class ""

                    _ ->
                        class "form-input-invalid"
                , value field.state.value
                , onInput FieldMsg
                , onBlur Blur
                , placeholder (Maybe.withDefault "" field.placeholder)
                ]
                []
            , span [ class "form-description" ] [ text (Maybe.withDefault "" field.description) ]
            ]
        , div []
            (List.map
                (\e ->
                    div [ class "form-hint" ] [ text e ]
                )
                field.hints
            )
        ]


textFieldUpdate : ctx -> String -> Value String TextFieldConfig -> Value String TextFieldConfig
textFieldUpdate _ msg state =
    { state | value = msg }


type alias SelectFieldConfig a =
    { display : a -> String
    , key : a -> String
    }


type alias SelectField form ctx a =
    Form.Field form a (SelectFieldConfig a) String String ctx (List a) (Html (FieldMsg String))


type alias SelectFieldState a =
    Value a (SelectFieldConfig a)


selectField : (ctx -> List a) -> SelectFieldConfig a -> a -> SelectField form ctx a
selectField ctx config value =
    { view = selectFieldView
    , state =
        { value = value
        , display = config.display
        , key = config.key
        }
    , update = selectFieldUpdate
    , hints = []
    , label = Nothing
    , placeholder = Nothing
    , description = Nothing
    , getContext = ctx
    , hinting = []
    , globalHinting = []
    }


selectFieldView : List a -> ViewConfig a (SelectFieldConfig a) String -> Html (FieldMsg String)
selectFieldView list field =
    let
        options =
            List.map
                (\a ->
                    option [ value (field.state.key a) ] [ text (field.state.display a) ]
                )
                list
    in
    div []
        [ label [ class "form-label" ]
            [ text (Maybe.withDefault "" field.label)
            , select
                [ onInput FieldMsg
                , onBlur Blur
                , class "form-control"
                ]
                options
            , span [] [ text (Maybe.withDefault "" field.description) ]
            ]
        , div []
            (List.map
                (\e ->
                    p [] [ text e ]
                )
                field.hints
            )
        ]


selectFieldUpdate : List a -> String -> Value a (SelectFieldConfig a) -> Value a (SelectFieldConfig a)
selectFieldUpdate list msg state =
    let
        find l =
            case l of
                [] ->
                    Nothing

                x :: xs ->
                    if state.key x == msg then
                        Just x

                    else
                        find xs

        res =
            find list
    in
    case res of
        Just thing ->
            { state | value = thing }

        Nothing ->
            state
