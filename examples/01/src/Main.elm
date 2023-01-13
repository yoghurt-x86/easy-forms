module Main exposing (..)

import Form exposing (Form)
import Form.SimpleFields
import Browser
import Html exposing (..)
import Html.Events exposing (..)

main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = \ _ -> Sub.none
    }


type alias Fruits =
    { fruits : List String }

type alias Model = 
    { form : Form.Form Input Input ValidInput Fruits
    , fruits : Fruits
    }



type alias Input = 
    { email : String
    , password : String
    , fruit : String
    }


type alias ValidInput = 
    { email : String
    , password : String
    , fruit : String
    }


type Msg 
    = FormMsg (Form Input Input ValidInput Fruits)
    | Submit 


form : Form Input Input ValidInput Fruits
form = 
    let notEmpty str = 
            if String.isEmpty str then 
                Err "Field is empty"
            else 
                Ok str

        email = 
            Form.SimpleFields.textField ""
                |> Form.withLabel "something"
                |> Form.withDescription "something more"

        password = 
            Form.SimpleFields.textField ""
                |> Form.withLabel "something"
                |> Form.withDescription "something more"

        fruits = 
            Form.SimpleFields.selectField 
                .fruits
                { display = identity
                , key = identity 
                }
                "Banana"
                |> Form.withLabel "fruits"
                |> Form.withDescription "something more"

    in
    Form.succeed Input ValidInput
        |> Form.appendValid 
            (.email >> notEmpty)
            email 
        |> Form.append password 
        |> Form.append fruits 


init : () -> (Model, Cmd Msg)
init _ = 
    ( { form = form, fruits = { fruits = [ "Banana", "Apple", "Orange"] } }
    , Cmd.none
    )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of 
        FormMsg fmsg ->
            ( { model | form = fmsg }
            , Cmd.none
            )

        Submit ->  
            case Form.valid model.form of
                Ok _ -> 
                    (model, Cmd.none)

                Err f -> 
                    ( { model | form = f }
                    , Cmd.none
                    )


view : Model -> Html Msg
view model = 
    div []    
        [ Html.map FormMsg <|
            div [] <|
                Form.view  
                    model.fruits
                    model.form
        , button 
            [ onClick Submit ] 
            [ text "submit" ]
        ]

