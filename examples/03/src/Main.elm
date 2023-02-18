module Main exposing (..)

import Browser
import Color exposing (Color)
import Date exposing (Date)
import File exposing (File)
import Form.Field as Field
import Form.Html as Form exposing (SimpleForm)
import Form.SimpleFields as Field
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { form : SimpleForm Input }


type alias Input =
    { email : String
    , password : String
    , repeatPassword : String
    , newsletter : Bool
    }


type Msg
    = FormMsg (SimpleForm Input)
    | Submit


form : SimpleForm Input
form =
    let
        email =
            Field.textField
                { textType = Field.Email }
                ""
                |> Field.withLabel "Email"
                |> Field.withPlaceholder "email@mail.com"
                |> Field.withDescription "We'll never share your email with anyone else."
                |> Field.withHints
                    [ Field.isEmail "Please add a correctly formatted email"
                    , Field.notEmpty "Email field is empty"
                    ]

        password =
            Field.textField
                { textType = Field.Password }
                ""
                |> Field.withLabel "Password"
                |> Field.withPlaceholder "Password"
                |> Field.withHints
                    [ Field.notEmpty "Add a password"
                    , \str ->
                        if (String.trim >> String.length) str >= 8 then
                            Ok ()

                        else
                            Err "Your password should be at least 8 characters"
                    ]

        repeatPassword =
            Field.textField
                { textType = Field.Password }
                ""
                |> Field.withLabel "Password"
                |> Field.withPlaceholder "Password"
                |> Field.withHints
                    [ Field.notEmpty "Please repeat your password"
                    ]
                |> Field.withGlobalHints
                    [ \_ valid ->
                        if valid.password == valid.repeatPassword then
                            Ok ()

                        else
                            Err "Please repeat the same password"
                    ]

        newsletter =
            Field.checkBox
                False
                |> Field.withLabel "Sign up for our newsletter"
                |> Field.withDescription "Stay up to date with our latest offers!"
    in
    Form.succeed Input Input
        |> Form.append email
        |> Form.append password
        |> Form.append repeatPassword
        |> Form.append newsletter


init : () -> ( Model, Cmd Msg )
init _ =
    ( { form = form }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormMsg fmsg ->
            ( { model | form = fmsg }
            , Cmd.none
            )

        Submit ->
            case Form.isValid () model.form of
                Ok _ ->
                    ( model, Cmd.none )

                Err f ->
                    ( { model | form = f }
                    , Cmd.none
                    )


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "padding" "4em"
        ]
        [ Html.form
            [ onSubmit Submit
            , class "form"
            ]
            [ div []
                (Form.view ()
                    model.form
                )
                |> Html.map FormMsg
            , button
                [ type_ "submit" ]
                [ text "Sign up" ]
            ]
        ]
