module Main exposing (..)


import Browser
import Form.Field as Field
import Form.Html as Form exposing (SimpleForm)
import Form.SimpleFields as Field
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Date exposing (Date)
import Color exposing (Color)
import File exposing (File)

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
    { text : String
    , textarea : String
    , password : String
    , select : Maybe String
    , color : Color
    , date : Maybe Date
    , number : Float
    , check : Bool
    , file : Maybe File
    }


type Msg
    = FormMsg (SimpleForm Input)
    | Submit


form : SimpleForm Input
form =
    let
        text =
            Field.textField { textType = Field.Text } ""
                |> Field.withLabel "Text"

        textArea =
            Field.textField
                { textType = Field.TextArea }
                ""
                |> Field.withLabel "Paragraph"

        password =
            Field.textField
                { textType = Field.Password }
                ""
                |> Field.withLabel "Password"
                |> Field.withPlaceholder "Password"

        select =
            Field.selectMaybeField
                [ "Banana"
                , "Apple"
                , "Orange"
                , "Kiwi"
                , "Pear"
                , "Mango"
                , "Passionfruit"
                ]
                { display = identity 
                , key = identity
                }
                Nothing
                |> Field.withLabel "Choose your favorite fruit"

        color = 
            Field.colorField 
                (Color.rgb 0.5 0.5 0.0)
                |> Field.withLabel "Color"

        date = 
            Field.dateField
                Nothing
                |> Field.withLabel "Date"

        number = 
            Field.rangeField
                { min = 0.0
                , max = 10.0
                , step = 2.5
                }
                0.0
                |> Field.withLabel "Number"
                |> Field.withDescription "Choose a number between 0 and 10"

        checkBox = 
            Field.checkBox 
                False
                |> Field.withLabel "Do you agree?"
                |> Field.withDescription "By agreeing you accept that you agree with our agreement."

        file = 
            Field.fileField 
                { accept = [ "*" ] }
                Nothing
                |> Field.withLabel "Upload file"

    in
    Form.succeed Input Input
        |> Form.append text
        |> Form.append textArea
        |> Form.append password
        |> Form.append select
        |> Form.append color
        |> Form.append date
        |> Form.append number
        |> Form.append checkBox
        |> Form.append file


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
                [ text "Submit" ]
            ]
        ]
