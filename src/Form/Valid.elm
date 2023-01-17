module Form.Valid exposing (..)

import Regex exposing (Regex)


type alias Validation a b error =
    a -> Result error b

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
