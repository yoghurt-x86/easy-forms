module Form.Field exposing
    ( Field
    , FieldMsg(..)
    , GlobalHint
    , Hint
    , Parser
    , Value
    , ViewConfig
    , isDefined
    , isEmail
    , isTrue
    , notEmpty
    , withDescription
    , withGlobalHints
    , withHints
    , withLabel
    , withPlaceholder
    )

{-| This module defines some of the tools to work with fields

## Label your field:
@docs withLabel, withPlaceholder, withDescription 

## Validations:
@docs Hint, GlobalHint, withHints, withGlobalHints, notEmpty, isTrue, isDefined, isEmail

## Types:
@docs Field, FieldMsg, Parser, Value, ViewConfig
-}

import Regex exposing (Regex)

{-| The internal field type.
-}
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


{-| Every field state needs to have a value
-}
type alias Value value state =
    { state | value : value }


{-| The record given to a field view function
-}
type alias ViewConfig value state hint =
    { state : Value value state
    , hints : List hint
    , label : Maybe String
    , placeholder : Maybe String
    , description : Maybe String
    }


{-| These messages can be used by the form system
-}
type FieldMsg msg
    = Blur
    | FieldMsg msg


{-| Add a label to your field. The field is put inside a label tag.
-}
withLabel :
    String
    -> Field form value state msg error ctx fieldCtx view
    -> Field form value state msg error ctx fieldCtx view
withLabel label field =
    { field | label = Just label }


{-| Add a placeholder value to a field.
-}
withPlaceholder :
    String
    -> Field form value state msg error ctx fieldCtx view
    -> Field form value state msg error ctx fieldCtx view
withPlaceholder placeholder field =
    { field | placeholder = Just placeholder }


{-| Add a short description under a field
-}
withDescription :
    String
    -> Field form value state msg error ctx fieldCtx view
    -> Field form value state msg error ctx fieldCtx view
withDescription description field =
    { field | description = Just description }


{-| Add hints to a field. When the field is updated all the hints will be tested and displayed if they don't pass.
-}
withHints :
    List (Hint hint value)
    -> Field form value state msg hint ctx fieldCtx view
    -> Field form value state msg hint ctx fieldCtx view
withHints hints field =
    { field | hinting = hints }


{-| Add a global hint
-}
withGlobalHints :
    List (GlobalHint ctx hint form)
    -> Field form value state msg hint ctx fieldCtx view
    -> Field form value state msg hint ctx fieldCtx view
withGlobalHints hints field =
    { field | globalHinting = hints }


{-| Parse a field to another value.
-}
type alias Parser a b hint =
    a -> Result hint b


{-| Hint
-}
type alias Hint hint a =
    a -> Result hint ()


{-| Global Hint
-}
type alias GlobalHint ctx hint form =
    ctx -> form -> Result hint ()


{-| Fails in case the string is empty or whitespace only
-}
notEmpty : hint -> Hint hint String
notEmpty hint value =
    if String.trim value |> String.isEmpty then
        Err hint

    else
        Ok ()


{-| Fails if false
-}
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


{-| Will use a regex to check if a string is in an email shape
-}
isEmail : hint -> Hint hint String
isEmail hint value =
    if Regex.contains validEmail value then
        Ok ()

    else
        Err hint


{-| Fails if the Maybe does not contain a value
-}
isDefined : hint -> Hint hint (Maybe a)
isDefined hint value =
    case value of
        Just _ ->
            Ok ()

        Nothing ->
            Err hint
