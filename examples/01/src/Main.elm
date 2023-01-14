module Main exposing (..)

import Form exposing (Form)
import Form.SimpleFields
import Form.Valid
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


type alias Model = 
    { form : Form.Form Input Input ValidInput ()
    }


type alias Input = 
    { email : String
    , password : String
    , repeatPassword : String
    }


type alias ValidInput = 
    { email : String
    , password : String
    , repeatPassword : String
    }


type Msg 
    = FormMsg (Form Input Input ValidInput ())
    | Submit 


form : Form Input Input ValidInput ()
form = 
    let match p1 p2 =
            if p1 == p2 then
                Ok p2

            else 
                Err ("Password has to match", [])

        email = 
            Form.SimpleFields.textField 
                { textType = Form.SimpleFields.Email }
                ""
                |> Form.withLabel "Email"
                |> Form.withPlaceholder "email@mail.com"

        password = 
            Form.SimpleFields.textField 
                { textType = Form.SimpleFields.Password }
                ""
                |> Form.withLabel "Password"
                |> Form.withPlaceholder "Password"

        repeatPassword = 
            Form.SimpleFields.textField 
                { textType = Form.SimpleFields.Password }
                ""
                |> Form.withLabel "Repeat password"

    in
    Form.succeed Input ValidInput
        |> Form.appendValid 
            (Form.Valid.combine 
                (   Form.Valid.notEmpty "Input an email"
                , [ Form.Valid.isEmail "Please input email in a correct format"
                  ]
                )
                |> Form.Valid.at .email
            )
            email 
        |> Form.appendValid 
            (.password >> Form.Valid.notEmpty "empty password")
            password
        |> Form.appendValid
            (\ result -> 
                match result.password result.repeatPassword
            )
            repeatPassword 


init : () -> (Model, Cmd Msg)
init _ = 
    ( { form = form }
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
                Form.view ()
                    model.form
        , button 
            [ onClick Submit ] 
            [ text "submit" ]
        ]

