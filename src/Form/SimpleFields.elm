module Form.SimpleFields exposing (..)

import Form exposing (FieldMsg(..), Value, ViewConfig)
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


type alias TextField ctx = 
    Form.Field String TextFieldConfig String String ctx ctx


type TextFieldState = 
    Value String TextFieldConfig
        

textField : TextFieldConfig -> String -> TextField ctx 
textField config value =
    { view = textFieldView
    , state =
        { value = value
        , textType = config.textType
        }
    , update = textFieldUpdate
    , error = []
    , label = Nothing
    , placeholder = Nothing
    , description = Nothing
    , getContext = identity
    }


defaultError : valid -> Result error valid
defaultError valid =
    Ok valid


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
                TextArea -> textarea
                _ -> input

    in
    div []
        [ label []
            [ text (Maybe.withDefault "" field.label)
            , element
                [ type_ textType
                , value field.state.value
                , onInput FieldMsg
                , onBlur Blur
                , placeholder (Maybe.withDefault "" field.placeholder)
                ]
                []
            ]
        , div [] 
            (List.map 
                (\ e -> 
                    p [] [ text e ]
                )
                field.error 
            )
        ]


textFieldUpdate : ctx -> String -> Value String TextFieldConfig -> Value String TextFieldConfig
textFieldUpdate _ msg state =
    { state | value = msg }


type alias SelectFieldConfig a =
    { display : a -> String
    , key : a -> String
    }


type alias SelectField ctx a =
    Form.Field a (SelectFieldConfig a) String String ctx (List a)


type alias SelectFieldState a =
    Value a (SelectFieldConfig a)


selectField : (ctx -> List a) -> SelectFieldConfig a -> a -> SelectField ctx a
selectField ctx config value =
    { view = selectFieldView
    , state =
        { value = value
        , display = config.display
        , key = config.key
        }
    , update = selectFieldUpdate
    , error = []
    , label = Nothing
    , placeholder = Nothing
    , description = Nothing
    , getContext = ctx
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
        [ label []
            [ text (Maybe.withDefault "" field.label)
            , select
                [ onInput FieldMsg
                , onBlur Blur
                ]
                options
            ]
        , div [] 
            (List.map 
                (\ e -> 
                    p [] [ text e ]
                )
                field.error 
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
