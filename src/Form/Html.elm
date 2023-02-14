module Form.Html exposing
    ( Form
    , FormT
    , SimpleForm
    , append
    , appendParsed
    , get
    , hardcoded
    , hardcodedParsed
    , isValid
    , succeed
    , view
    , viewSimple
    )

{-| This is where you compose your forms!
This particular form package is compatible with [elm/html][]

[elm/html] : https://package.elm-lang.org/packages/elm/html/latest/ 

# Definition
@docs SimpleForm, Form, FormT

# Using a form:
@docs viewSimple, view, get, isValid

# Composing forms:
@docs append, succeed, hardcoded, appendParsed, hardcodedParsed
-}

import Html exposing (Html)
import Form.Field
    exposing
        ( Field
        , FieldMsg(..)
        , Parser
        )
import Form.Internals
    exposing
        ( formView
        , hardcodedField
        , hardcodedView
        , validateField
        )


{-| Simplest form shape
-}
type alias SimpleForm value =
    FormT value value value ()

{-| Expanded form shape
-}
type alias Form value validated context =
    FormT value value validated context


{-| Original Form type used internally
-}
type FormT value valid validated context
    = Form (FormRecord value valid validated context)


type alias FormRecord value valid validated context =
    { value : value
    , validated : context -> valid -> Result (FormT value valid validated context) validated
    , view : context -> valid -> List (Html (FormT value valid validated context))
    }


{-| Check if all fields pass all hints and parse to their resulting type.
-}
isValid : context -> Form value validated context -> Result (Form value validated context) validated
isValid context (Form form) =
    form.validated context form.value


{-| Get the current input value
-}
get : Form value validated context -> value
get (Form form) =
    form.value


{-| Render a form that requires a context

This returns a list of Html. Each item in the list corresponds to a field in the form.

    div [] 
        ( view () form )

-}
view : context -> Form value validated context -> List (Html (Form value validated context))
view context (Form form) =
    form.view
        context
        form.value
        |> List.reverse


{-| View function for the form

This returns a list of Html. Each item in the list corresponds to a field in the form.

    div [] 
        ( view form )

-}
viewSimple : SimpleForm value -> List (Html (SimpleForm value))
viewSimple (Form form) =
    form.view () form.value
        |> List.reverse


{-| Start of the pipeline
-}
succeed : value -> validated -> FormT value valid validated context
succeed value validated =
    Form
        { value = value
        , validated = \_ _ -> Ok validated
        , view = \_ _ -> []
        }


{-| Append another field in your form pipeline!
-}
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


{-| Append a hardcoded value in your form pipeline!
-}
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


{-| Append another field in your form pipeline. 

This time adding a parser step. 
-}
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


{-| Append a hardcoded value in your form pipeline!

This time adding a parser step. 
-}
hardcodedParsed :
    Parser valid b error
    -> a
    -> FormT (a -> value) valid (b -> validated) context
    -> FormT value valid validated context
hardcodedParsed validate value (Form form) =
    Form
        { value = form.value value
        , validated =
            \ctx val ->
                hardcodedField (validate val) form Form (hardcodedParsed validate value) ctx val
        , view =
            \ctx val ->
                hardcodedView form Html.map (hardcodedParsed validate value) ctx val
        }
