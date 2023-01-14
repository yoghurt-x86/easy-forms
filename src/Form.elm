module Form exposing (..)


import Html exposing (Html)
import Form.Valid exposing (Validation)


type Form value valid validated context =
    Form (FormRecord value valid validated context)

type FieldMsg msg
    = Blur
    | FieldMsg msg


type Msg value = 
    Msg value

type alias FormRecord value valid validated context =
    { value : value 
    , validated : valid -> Result (Form value valid validated context) validated
    , view : context -> valid -> List (Html (Form value valid validated context))
    }

type alias Value value state =
    { state | value : value }

succeed : value -> validated -> Form value valid validated context
succeed value validated = 
    Form 
        { value = value 
        , validated = always (Ok validated)
        , view = \ _ _ -> []
        }

append : Field a state msg error context localCtx
   -> Form (a -> value) valid (a -> value) context -> Form value valid value context
append field (Form form) = 
    Form 
        { value = form.value field.state.value
        , validated = \ val ->
            case form.validated val of 
                Ok o -> Ok (o field.state.value)
                Err f -> Err (append field f)
        , view = \ ctx val ->
              let fieldView = 
                    field.view 
                        (field.getContext ctx) 
                        { state = field.state 
                        , error = field.error 
                        , label = field.label 
                        , description = field.description
                        , placeholder = field.placeholder
                        }
                        |> Html.map 
                            (\ f -> 
                                case f of 
                                    FieldMsg fmsg -> 
                                        let state = field.update (field.getContext ctx) fmsg field.state
                                        in
                                        append { field | state = state } (Form form)

                                    Blur ->
                                        append field (Form form)
                            )
              in
              fieldView :: 
                (form.view ctx val
                    |> List.map 
                        (Html.map 
                            (\ forms -> 
                                append field forms 
                            )
                        ) 
                )
        }



appendValid : Validation valid b error 
    -> Field a state msg error context localCtx
    -> Form (a -> value) valid (b -> validated) context
    -> Form value valid validated context
appendValid validate field (Form form) = 
    Form 
        { value = form.value field.state.value
        , validated =  \ val ->
            let this = validate val
            in
            case (this, form.validated val) of
                (Ok ok, Ok f ) -> Ok (f ok)
                (Err (e, es), Ok _ ) -> Err (appendValid validate { field | error = e :: es } (Form form) )
                (Ok _, Err f ) -> Err (appendValid validate { field | error = [] } f)
                (Err (e, es), Err f ) -> Err (appendValid validate { field | error = e :: es } f)
        , view = \ ctx val ->
              let fieldView = 
                    field.view 
                        (field.getContext ctx)
                        { state = field.state 
                        , error = field.error 
                        , label = field.label 
                        , description = field.description
                        , placeholder = field.placeholder
                        }
                        |> Html.map 
                            (\ f -> 
                                case f of 
                                    FieldMsg fmsg -> 
                                        let state = field.update (field.getContext ctx) fmsg field.state
                                        in
                                        appendValid validate { field | state = state } (Form form)

                                    Blur ->
                                        case validate val of 
                                            Ok _ -> appendValid validate { field | error = [] } (Form form)
                                            Err (e, es) -> appendValid validate { field | error = e :: es } (Form form)
                            )
              in
              fieldView :: 
                (form.view ctx val
                    |> List.map 
                        (Html.map 
                            (\ forms -> 
                                appendValid validate field forms 
                            )
                        ) 
                )
        }


valid : Form value value validated context -> Result (Form value value validated context) validated
valid (Form form) =
    form.validated form.value


view : context -> Form value value validated context-> List (Html (Form value value validated context))
view context (Form form) =
    form.view 
        context 
        form.value
        |> List.reverse


type alias ViewConfig value state error = 
    { state : Value value state
    , error : List error
    , label : Maybe String
    , placeholder : Maybe String
    , description : Maybe String
    }


type alias Field value state msg error ctx fieldCtx =
    { view : fieldCtx -> ViewConfig value state error -> (Html (FieldMsg msg))
    , state : Value value state
    , update : fieldCtx -> msg -> Value value state -> Value value state 
    , error : List error
    , label : Maybe String
    , placeholder : Maybe String
    , description : Maybe String
    , getContext : ctx -> fieldCtx
    }


withLabel : String 
    -> Field value state msg error ctx fieldCtx
    -> Field value state msg error ctx fieldCtx
withLabel label field = 
    { field | label = Just label }

withPlaceholder : String 
    -> Field value state msg error ctx fieldCtx
    -> Field value state msg error ctx fieldCtx
withPlaceholder placeholder field = 
    { field | placeholder = Just placeholder }


withDescription : String 
    -> Field value state msg error ctx fieldCtx
    -> Field value state msg error ctx fieldCtx
withDescription description field = 
    { field | description = Just description }

