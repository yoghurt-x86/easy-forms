module Main exposing (..)

import Form.Html as Form exposing (SimpleForm)
import Form.SimpleFields as Field
import Form.Field as Field
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = \ _ -> Sub.none
    }


type alias Model = 
    { form : SimpleForm Input }


type alias Input = 
    { email : String
    , password : String
    , repeatPassword : String
    }

type Msg 
    = FormMsg (SimpleForm Input)
    | Submit 


form : SimpleForm Input
form = 
    let match p1 p2 =
            if p1 == p2 then
                Ok ()

            else 
                Err "Password has to match"

        email = 
            Field.textField 
                { textType = Field.Email }
                ""
                |> Field.withLabel "Email"
                |> Field.withPlaceholder "email@mail.com"
                |> Field.withDescription "We'll never share your email with anyone else."
                |> Field.withHints 
                    [ Field.notEmpty "Input email"
                    , Field.isEmail "Input correct email"
                    ]

        password = 
            Field.textField 
                { textType = Field.Password }
                ""
                |> Field.withLabel "Password"
                |> Field.withPlaceholder "Password"

        repeatPassword = 
            Field.textField 
                { textType = Field.Password }
                ""
                |> Field.withLabel "Repeat password"
                |> Field.withPlaceholder "Repeat password"
                |> Field.withGlobalHints 
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
