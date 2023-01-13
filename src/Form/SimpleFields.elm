module Form.SimpleFields exposing (..)


import Form  exposing (Value, FieldMsg(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


textField : String -> Form.Field String {} String String ctx ctx
textField value = 
    { view = textFieldView 
    , state = { value = value }
    , update = textFieldUpdate
    , error = Nothing
    , label = Nothing
    , description = Nothing
    , getContext = identity
    }
    

defaultError : valid -> Result error valid 
defaultError valid = 
    Ok valid

textFieldView : ctx -> Value String {} -> Maybe String -> Maybe String -> Maybe String -> Html (FieldMsg String)
textFieldView _ state error label_string description = 
    div [] 
        [ label [] 
            [ text (Maybe.withDefault "" label_string)
            , input 
                [ type_ "text" 
                , value state.value
                , onInput FieldMsg
                , onBlur Blur 
                ] 
                []
            ]
        , case error of 
            Just e ->
                p [] [ text e ]

            Nothing ->
                text ""
        ]


textFieldUpdate : ctx -> String -> Value String {} -> Value String {}
textFieldUpdate _ msg _ =
    { value = msg }


type alias SelectFieldConfig a =
    { display : a -> String
    , key : a -> String
    }

selectField : (ctx -> List a) -> SelectFieldConfig a -> a -> Form.Field a (SelectFieldConfig a) String String ctx (List a)
selectField ctx config value = 
    { view = selectFieldView 
    , state = 
        { value = value 
        , display = config.display
        , key = config.key
        }
    , update = selectFieldUpdate
    , error = Nothing
    , label = Nothing
    , description = Nothing
    , getContext = ctx
    }


selectFieldView : List a -> Value a (SelectFieldConfig a) -> Maybe String -> Maybe String -> Maybe String -> Html (FieldMsg String)
selectFieldView list state error label_string description = 
    let options = 
            List.map 
                (\ a -> 
                    option [ value (state.key a) ] [ text (state.display a) ] 
                )
                list
                
    in 
    div [] 
        [ label [] 
            [ text (Maybe.withDefault "" label_string)
            , select 
                [ onInput FieldMsg
                , onBlur Blur 
                ] 
                options
            ]
        , case error of 
            Just e ->
                p [] [ text e ]

            Nothing ->
                text ""
        ]


selectFieldUpdate : List a -> String -> Value a (SelectFieldConfig a) -> Value a (SelectFieldConfig a)
selectFieldUpdate list msg state = 
    let find l = 
            case l of
                [] -> Nothing
                x :: xs ->
                    if state.key x == msg then 
                        Just x 
                    else 
                        find xs
        res = find list
    in
    case res of
        Just thing ->
            { state | value = thing } 
        Nothing ->
            state

