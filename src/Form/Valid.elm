module Form.Valid exposing (..)

import Regex exposing (Regex)


type alias NonEmptyList a =
    (a, List a)

type alias Validation a b error =
    a -> Result (NonEmptyList error) b


notEmpty : error -> Validation String String error
notEmpty error value =
    if String.trim value |> String.isEmpty then 
        Err (error, [])

    else
        Ok value


validEmail : Regex
validEmail =
    "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
        |> Regex.fromStringWith { caseInsensitive = True, multiline = False }
        |> Maybe.withDefault Regex.never


isEmail : error -> Validation String String error
isEmail error value = 
    if Regex.contains validEmail value then
        Ok value 
    else 
        Err (error, [])


combine : NonEmptyList (a -> Result (NonEmptyList error) b) -> Validation a b error
combine (x, xs) value = 
    List.foldl 
        (\ valid acc ->
            case (acc, valid value) of 
                (Ok a, Ok b) -> Ok a 
                (Err (e1, errors1), Err (e2, errors2)) -> Err (e1, errors1 ++ (e2 ::errors2))
                (Err e, Ok _ ) -> Err e
                (Ok _, Err e) -> Err e
        )
        (x value)
        xs


at : (a -> b) -> Validation b c error -> Validation a c error 
at f validation =
    f >> validation


andMap : Validation b c error -> Validation b b error -> Validation b c error
andMap try validation value = 
    case (try value, validation value) of 
        (Ok new, Ok _) -> Ok new
        (Err (e1, errors1), Err (e2, errors2)) -> Err (e1, errors1 ++ (e2 ::errors2))
        (Ok _, Err e) -> Err e
        (Err e, Ok _) -> Err e

