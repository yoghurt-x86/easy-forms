module Form.Html exposing 
    ( Form 
    , viewSimple
    , SimpleForm
    , succeed 
    , append
    , hardcoded
    , appendParsed
    , hardcodedParsed
    , isValid
    , get
    , view
    )

import Form.Field
    exposing
        ( Field
        , FieldMsg(..)
        , Parser
        )
import Form.Internals
    exposing
        ( formView
        , validateField
        , hardcodedField
        , hardcodedView
        )
import Html exposing (Html)


type alias SimpleForm value =
    FormT value value value ()


type alias Form value validated context =
    FormT value value validated context


type FormT value valid validated context
    = Form (FormRecord value valid validated context)


type alias FormRecord value valid validated context =
    { value : value
    , validated : context -> valid -> Result (FormT value valid validated context) validated
    , view : context -> valid -> List (Html (FormT value valid validated context))
    }


isValid : context -> Form value validated context -> Result (Form value validated context) validated
isValid context (Form form) =
    form.validated context form.value


get : Form value validated context -> value
get (Form form) =
    form.value


view : context -> Form value validated context -> List (Html (Form value validated context))
view context (Form form) =
    form.view
        context
        form.value
        |> List.reverse


viewSimple : SimpleForm value -> List (Html (SimpleForm value))
viewSimple (Form form) =
    form.view () form.value 
        |> List.reverse


succeed : value -> validated -> FormT value valid validated context
succeed value validated =
    Form
        { value = value
        , validated = \_ _ -> Ok validated
        , view = \_ _ -> []
        }


append :
    Field valid a state msg error context localCtx (Html (FieldMsg msg))
    -> FormT (a -> value) valid (a -> value) context
    -> FormT value valid value context
append field (Form form) =
    Form
        { value = form.value field.state.value
        , validated =
            validateField (Ok field.state.value) form Form field append
        , view =
            \ctx val ->
                formView field form Html.map Html.map Form append ctx val
        }


hardcoded :
    a
    -> FormT (a -> value) valid (a -> value) context
    -> FormT value valid value context
hardcoded value (Form form) =
    Form
        { value = form.value value
        , validated =
            hardcodedField (Ok value) form Form (hardcoded value)
        , view =
            \ctx val ->
                hardcodedView form Html.map (hardcoded value) ctx val
        }


appendParsed :
    Parser valid b error
    -> Field valid a state msg error context localCtx (Html (FieldMsg msg))
    -> FormT (a -> value) valid (b -> validated) context
    -> FormT value valid validated context
appendParsed validate field (Form form) =
    Form
        { value = form.value field.state.value
        , validated =
            \ctx val ->
                validateField (validate val) form Form field (appendParsed validate) ctx val
        , view =
            \ctx val ->
                formView field form Html.map Html.map Form (appendParsed validate) ctx val
        }


hardcodedParsed :
    Parser valid b error
    -> a
    -> FormT (a -> value) valid (b -> validated) context
    -> FormT value valid validated context
hardcodedParsed validate value (Form form) =
    Form
        { value = form.value value
        , validated = \ ctx val ->
            hardcodedField (validate val) form Form (hardcodedParsed validate value) ctx val
        , view =
            \ctx val ->
                hardcodedView form Html.map (hardcodedParsed validate value) ctx val
        }
