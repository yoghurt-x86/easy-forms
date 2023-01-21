module Form.SimpleFields exposing (..)

import Color exposing (Color)
import Date exposing (Date)
import File exposing (File)
import Form.Field as Form exposing (Field, FieldMsg(..), Value, ViewConfig)
import Hex
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json


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


type alias TextFieldState =
    Value String TextFieldConfig


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
                    type_ "text"

                TextArea ->
                    class ""

                Password ->
                    type_ "password"

                Email ->
                    type_ "email"

                Search ->
                    type_ "search"

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
                [ textType
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


type alias SelectField form ctx localCtx a =
    Form.Field form a (SelectFieldConfig a) String String ctx localCtx (Html (FieldMsg String))


type alias SelectFieldState a =
    Value a (SelectFieldConfig a)


selectMaybeField : List a -> SelectFieldConfig a -> Maybe a -> SelectField form ctx ctx (Maybe a)
selectMaybeField list config value =
    { view = \_ -> selectFieldView (Nothing :: List.map Just list)
    , state =
        { value = value
        , display = (Maybe.map config.display) >> (Maybe.withDefault "")
        , key = (Maybe.map config.key) >> (Maybe.withDefault "")
        }
    , update = \ _ -> selectFieldUpdate (Nothing :: List.map Just list)
    , hints = []
    , label = Nothing
    , placeholder = Nothing
    , description = Nothing
    , getContext = identity
    , hinting = []
    , globalHinting = []
    }

selectField : List a -> SelectFieldConfig a -> a -> SelectField form ctx ctx a
selectField list config value =
    { view = \_ -> selectFieldView list
    , state =
        { value = value
        , display = config.display
        , key = config.key
        }
    , update = \ _ -> selectFieldUpdate list
    , hints = []
    , label = Nothing
    , placeholder = Nothing
    , description = Nothing
    , getContext = identity
    , hinting = []
    , globalHinting = []
    }

selectContextualField : (ctx -> List a) -> SelectFieldConfig a -> a -> SelectField form ctx (List a) a
selectContextualField ctx config value =
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
                    option 
                        [ value (field.state.key a) 
                        , if (field.state.key a) == (field.state.key field.state.value) then 
                            selected True 
                          else 
                            selected False
                        ] 
                        [ text (field.state.display a) ]
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


inputDecoder : (String -> Json.Decoder b) -> Html.Attribute b
inputDecoder decoder =
    stopPropagationOn "input"
        (Json.map (\x -> ( x, True ))
            (targetValue
                |> Json.andThen
                    decoder
            )
        )


type alias DateFieldConfig =
    {}


type alias DateField form ctx =
    Field form (Maybe Date) DateFieldConfig (Maybe Date) String ctx ctx (Html (FieldMsg (Maybe Date)))


type alias DateFieldState =
    Value (Maybe Date) DateFieldConfig


dateField : (Maybe Date) -> DateField form ctx
dateField value =
    { view = dateFieldView
    , state =
        { value = value }
    , update = dateFieldUpdate
    , hints = []
    , label = Nothing
    , placeholder = Nothing
    , description = Nothing
    , getContext = identity
    , hinting = []
    , globalHinting = []
    }


dateFieldView : ctx -> ViewConfig (Maybe Date) DateFieldConfig String -> Html (FieldMsg (Maybe Date))
dateFieldView _ field =
    let
        datestring date =
            (Date.year date 
                |> String.fromInt
                |> String.padLeft 4 '0'
            )
                ++ "-"
                ++ (Date.monthNumber date
                        |> String.fromInt
                        |> String.padLeft 2 '0'
                   )
                ++ "-"
                ++ (Date.day date
                        |> String.fromInt
                        |> String.padLeft 2 '0'
                   )
    in
    div []
        [ label [ class "form-label" ]
            [ text (Maybe.withDefault "" field.label)
            , input
                [ type_ "date"
                , class "form-input"
                , case field.hints of
                    [] ->
                        class ""

                    _ ->
                        class "form-input-invalid"
                , Maybe.map (value << datestring)
                    field.state.value 
                    |> Maybe.withDefault (class "")
                , inputDecoder
                    (\str ->
                        case Date.fromIsoString str of
                            Ok date ->
                                Json.succeed (FieldMsg (Just date))

                            Err _ ->
                                Json.succeed (FieldMsg Nothing)
                    )
                , onBlur Blur
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


dateFieldUpdate : ctx -> Maybe Date -> Value (Maybe Date) DateFieldConfig -> Value (Maybe Date) DateFieldConfig
dateFieldUpdate _ msg state =
    { state | value = msg }


type alias RangeFieldConfig =
    { min : Float
    , max : Float
    , step : Float
    }


type alias RangeField form ctx =
    Field form Float RangeFieldConfig Float String ctx ctx (Html (FieldMsg Float))


type alias RangeFieldState =
    Value Float RangeFieldConfig


rangeField : RangeFieldConfig -> Float -> RangeField form ctx
rangeField config value =
    { view = rangeFieldView
    , state =
        { value = value
        , min = config.min
        , max = config.max
        , step = config.step
        }
    , update = rangeFieldUpdate
    , hints = []
    , label = Nothing
    , placeholder = Nothing
    , description = Nothing
    , getContext = identity
    , hinting = []
    , globalHinting = []
    }


rangeFieldView : ctx -> ViewConfig Float RangeFieldConfig String -> Html (FieldMsg Float)
rangeFieldView _ field =
    div []
        [ label [ class "form-label" ]
            [ text (Maybe.withDefault "" field.label)
            , input
                [ type_ "range"
                , class "form-input"
                , case field.hints of
                    [] ->
                        class ""

                    _ ->
                        class "form-input-invalid"
                , value (String.fromFloat field.state.value)
                , inputDecoder
                    (\str ->
                        case String.toFloat str of
                            Just num ->
                                Json.succeed (FieldMsg num)

                            Nothing ->
                                Json.fail "Could not parse number"
                    )
                , onBlur Blur
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


rangeFieldUpdate : ctx -> Float -> Value Float RangeFieldConfig -> Value Float RangeFieldConfig
rangeFieldUpdate _ msg state =
    { state | value = msg }


type alias ColorFieldConfig = {}

type alias ColorField form ctx =
    Field form Color ColorFieldConfig Color String ctx ctx (Html (FieldMsg Color))


type alias ColorFieldState =
    Value Color ColorFieldConfig


colorField : Color -> ColorField form ctx
colorField value =
    { view = colorFieldView
    , state =
        { value = value }
    , update = colorFieldUpdate
    , hints = []
    , label = Nothing
    , placeholder = Nothing
    , description = Nothing
    , getContext = identity
    , hinting = []
    , globalHinting = []
    }


colorFieldView : ctx -> ViewConfig Color ColorFieldConfig String -> Html (FieldMsg Color)
colorFieldView _ field =
    div []
        [ label [ class "form-label" ]
            [ text (Maybe.withDefault "" field.label)
            , input
                [ type_ "color"
                , class "form-input"
                , case field.hints of
                    [] ->
                        class ""

                    _ ->
                        class "form-input-invalid"
                , let
                    c =
                        Color.toRgba field.state.value

                    hex val =
                        String.padLeft 2 '0' <| 
                            Hex.toString (round (val * 255.0))
                  in
                  value <|
                    "#"
                        ++ hex c.red
                        ++ hex c.green
                        ++ hex c.blue
                , inputDecoder
                    (\str ->
                        let r = String.slice 1 3 str
                            g = String.slice 3 5 str
                            b = String.slice 5 7 str
                        in
                        case 
                            Result.map3 Color.rgb255
                                (Hex.fromString r)
                                (Hex.fromString g)
                                (Hex.fromString b)
                        of
                            Ok color ->
                                Json.succeed (FieldMsg color)

                            Err e ->
                                Json.fail e
                    )
                , onBlur Blur
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


colorFieldUpdate : ctx -> Color -> Value Color ColorFieldConfig -> Value Color ColorFieldConfig
colorFieldUpdate _ msg state =
    { state | value = msg }


type alias CheckFieldConfig = {}

type alias CheckField form ctx =
    Field form Bool CheckFieldConfig Bool String ctx ctx (Html (FieldMsg Bool))


type alias CheckFieldState =
    Value Bool CheckFieldConfig


checkBox : Bool -> CheckField form ctx
checkBox value =
    { view = checkFieldView
    , state =
        { value = value }
    , update = checkFieldUpdate
    , hints = []
    , label = Nothing
    , placeholder = Nothing
    , description = Nothing
    , getContext = identity
    , hinting = []
    , globalHinting = []
    }


checkFieldView : ctx -> ViewConfig Bool CheckFieldConfig String -> Html (FieldMsg Bool)
checkFieldView _ field =
    div []
        [ label [ class "form-label" ]
            [input
                [ type_ "checkbox"
                , class "form-input"
                , case field.hints of
                    [] ->
                        class ""

                    _ ->
                        class "form-input-invalid"
                , checked field.state.value
                , onCheck FieldMsg 
                , onBlur Blur
                ]
                []
            , text (Maybe.withDefault "" field.label)
            ]
        , span [ class "form-description" ] [ text (Maybe.withDefault "" field.description) ]
        , div []
            (List.map
                (\e ->
                    div [ class "form-hint" ] [ text e ]
                )
                field.hints
            )
        ]


checkFieldUpdate : ctx -> Bool -> Value Bool CheckFieldConfig -> Value Bool CheckFieldConfig
checkFieldUpdate _ msg state =
    { state | value = msg }


type alias FileFieldConfig = 
    { accept : List String
    }

type alias FileField form ctx =
    Field form (Maybe File) FileFieldConfig (Maybe File) String ctx ctx (Html (FieldMsg (Maybe File)))


type alias FileFieldState =
    Value (Maybe File) FileFieldConfig


fileField : FileFieldConfig -> Maybe File -> FileField form ctx
fileField config value =
    { view = fileFieldView
    , state =
        { value = value 
        , accept = config.accept
        }
    , update = fileFieldUpdate
    , hints = []
    , label = Nothing
    , placeholder = Nothing
    , description = Nothing
    , getContext = identity
    , hinting = []
    , globalHinting = []
    }


fileFieldView : ctx -> ViewConfig (Maybe File) FileFieldConfig String -> Html (FieldMsg (Maybe File))
fileFieldView _ field =
    div []
        [ label [ class "form-label" ]
            [ text (Maybe.withDefault "" field.label)
            , input
                [ type_ "file"
                , class "form-input"
                , case field.hints of
                    [] ->
                        class ""

                    _ ->
                        class "form-input-invalid"
                , accept 
                    ( List.intersperse ", " field.state.accept
                        |> List.foldl (++) ""
                    )
                , stopPropagationOn "change"
                    (Json.map (\x -> ( x, True ))
                        (Json.at ["target", "files" ]
                            (Json.list File.decoder)
                                |> Json.map ( \ l -> 
                                    case l of 
                                        [] -> FieldMsg Nothing
                                        x :: _ -> FieldMsg (Just x)
                            )
                        )

                    )
                , onBlur Blur
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


fileFieldUpdate : ctx -> Maybe File -> Value (Maybe File) FileFieldConfig -> Value (Maybe File) FileFieldConfig
fileFieldUpdate _ msg state =
    { state | value = msg }
