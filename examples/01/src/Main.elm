module Main exposing (..)

import Form exposing (Form)
import Form.SimpleFields
import Form.Valid
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Form exposing (withDescription)


main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = \ _ -> Sub.none
    }


type alias Model = 
    { form : Form.Form Input Input Input () }


type alias Input = 
    { email : String
    , password : String
    , repeatPassword : String
    }

type Msg 
    = FormMsg (Form Input Input Input ())
    | Submit 


form : Form Input Input Input ()
form = 
    let match p1 p2 =
            if p1 == p2 then
                Ok ()

            else 
                Err "Password has to match"

        email = 
            Form.SimpleFields.textField 
                { textType = Form.SimpleFields.Email }
                ""
                |> Form.withLabel "Email"
                |> Form.withPlaceholder "email@mail.com"
                |> Form.withDescription "We'll never share your email with anyone else."
                |> Form.withHints 
                    [ Form.Valid.notEmpty "Input email"
                    , Form.Valid.isEmail "Input correct email"
                    ]

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
                |> Form.withPlaceholder "Repeat password"
                |> Form.withGlobalHints 
                    [ \ _ val ->
                        match val.password val.repeatPassword
                    ]

    in
    Form.succeed Input Input
        |> Form.append email
        |> Form.append password
        |> Form.append repeatPassword 


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
            case Form.isValid () model.form of
                Ok _ -> 
                    (model, Cmd.none)

                Err f -> 
                    ( { model | form = f }
                    , Cmd.none
                    )


view : Model -> Html Msg
view model = 
    div [ style "display" "flex"
        , style "justify-content" "center"
        , style "padding" "4em"
        ] 

        [ Html.form 
            [ onSubmit Submit
            , class "form" 
            ]    
            [ div [] 
                ( Form.view ()
                    model.form
                )
                |> Html.map FormMsg 
            , button 
                [ type_ "submit" ] 
                [ text "Submit" ]
            ]
        ]
