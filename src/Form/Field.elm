module Form.Field exposing
    ( Field
    , FieldMsg(..)
    , isDefined
    , GlobalHint
    , Hint
    , Mapped
    , Value
    , ViewConfig
    , isEmail
    , isTrue
    , notEmpty
    , withDescription
    , withGlobalHints
    , withHints
    , withLabel
    , withPlaceholder
    )

import Regex exposing (Regex)


type alias Field form value state msg hint ctx fieldCtx view =
    { view : fieldCtx -> ViewConfig value state hint -> view
    , state : Value value state
    , update : fieldCtx -> msg -> Value value state -> Value value state
    , hints : List hint
    , label : Maybe String
    , placeholder : Maybe String
    , description : Maybe String
    , getContext : ctx -> fieldCtx
    , hinting : List (Hint hint value)
    , globalHinting : List (GlobalHint ctx hint form)
    }


type alias Value value state =
    { state | value : value }


type alias ViewConfig value state hint =
    { state : Value value state
    , hints : List hint
    , label : Maybe String
    , placeholder : Maybe String
    , description : Maybe String
    }


type FieldMsg msg
    = Blur
    | FieldMsg msg


withLabel :
    String
    -> Field form value state msg error ctx fieldCtx view
    -> Field form value state msg error ctx fieldCtx view
withLabel label field =
    { field | label = Just label }


withPlaceholder :
    String
    -> Field form value state msg error ctx fieldCtx view
    -> Field form value state msg error ctx fieldCtx view
withPlaceholder placeholder field =
    { field | placeholder = Just placeholder }


withDescription :
    String
    -> Field form value state msg error ctx fieldCtx view
    -> Field form value state msg error ctx fieldCtx view
withDescription description field =
    { field | description = Just description }


withHints :
    List (Hint hint value)
    -> Field form value state msg hint ctx fieldCtx view
    -> Field form value state msg hint ctx fieldCtx view
withHints hints field =
    { field | hinting = hints }


withGlobalHints :
    List (GlobalHint ctx hint form)
    -> Field form value state msg hint ctx fieldCtx view
    -> Field form value state msg hint ctx fieldCtx view
withGlobalHints hints field =
    { field | globalHinting = hints }


type alias Mapped a b hint =
    a -> Result hint b


type alias Hint hint a =
    a -> Result hint ()


type alias GlobalHint ctx hint form =
    ctx -> form -> Result hint ()


notEmpty : hint -> Hint hint String
notEmpty hint value =
    if String.trim value |> String.isEmpty then
        Err hint

    else
        Ok ()


isTrue : hint -> Hint hint Bool
isTrue hint value =
    if value then
        Err hint

    else
        Ok ()


validEmail : Regex
validEmail =
    "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
        |> Regex.fromStringWith { caseInsensitive = True, multiline = False }
        |> Maybe.withDefault Regex.never


isEmail : hint -> Hint hint String
isEmail hint value =
    if Regex.contains validEmail value then
        Ok ()

    else
        Err hint


isDefined : hint -> Hint hint (Maybe a)
isDefined hint value =
    case value of 
        Just _ -> 
            Ok()
        
        Nothing -> 
            Err hint

