module Form exposing (..)


import Html exposing (Html)
import Form.Valid exposing (Validation)
import Element exposing (Element)


type Form value valid validated context =
    Form (FormRecord value valid validated context)

type ElementForm value valid validated context =
    ElementForm (ElementFormRecord value valid validated context)

type FieldMsg msg
    = Blur
    | FieldMsg msg


type Msg value = 
    Msg value


type alias FormRecord value valid validated context =
    { value : value 
    , validated : context -> valid -> Result (Form value valid validated context) validated
    , view : context -> valid -> List (Html (Form value valid validated context))
    }

type alias ElementFormRecord value valid validated context =
    { value : value 
    , validated : context -> valid -> Result (ElementForm value valid validated context) validated
    , view : context -> valid -> List (Element (ElementForm value valid validated context))
    }



type alias Value value state =
    { state | value : value }


succeed : value -> validated -> Form value valid validated context
succeed value validated = 
    Form 
        { value = value 
        , validated = \ _ _ -> Ok validated
        , view = \ _ _ -> []
        }

checkHints : ctx -> form -> Field form value state msg hint ctx fieldCtx view -> List hint
checkHints ctx val field =
    let local = List.foldl 
                (\ hint hints ->
                    case hint field.state.value of
                        Ok _ -> hints
                        Err e -> e :: hints
                )
                [] field.hinting

        global = List.foldl 
                (\ hint hints ->
                    case hint ctx val of
                        Ok _ -> hints
                        Err e -> e :: hints
                )
                [] field.globalHinting
    in 
    local ++ global


validateField : 
    Result hint fieldval   
    -> { form | validated : ctx -> val -> Result form2 (fieldval -> validated) }
    -> ({ form | validated : ctx -> val -> Result form2 (fieldval -> validated) } -> form2)
    -> Field val value state msg hint ctx lctx html  
    -> (Field val value state msg hint ctx lctx html -> form2 -> continue)
    -> ctx 
    -> val
    -> Result continue validated 
validateField fieldVal form makeForm field continue ctx val =
    let hints = checkHints ctx val field
        valid = case (fieldVal, hints) of
                (Ok v, []) -> Ok v
                (Err e, []) -> Err [e]
                (Err e, es) -> Err (e :: es)
                (Ok _, es) -> Err es
    in
    case (valid, form.validated ctx val) of
        (Ok ok, Ok f ) -> Ok (f ok)
        (Err e, Ok _ ) -> Err (continue { field | hints = e } (makeForm form) )
        (Ok _, Err f ) -> Err (continue { field | hints = [] } f)
        (Err e, Err f ) -> Err (continue { field | hints = e } f)


append : 
   Field valid a state msg error context localCtx (Html (FieldMsg msg))
   -> Form (a -> value) valid (a -> value) context 
   -> Form value valid value context
append field (Form form) = 
    Form 
        { value = form.value field.state.value
        , validated = 
            validateField (Ok field.state.value) form Form field (append )
        , view = \ ctx val ->
            formView field form Html.map Html.map Form append ctx val 
        }


formView : 
    Field valid a state msg error ctx localCtx html8 
    -> { formRecord | view : ctx -> valid -> List html6 }
    -> ((FieldMsg msg -> continue) -> html8 -> html5)
    -> ((form -> continue) -> html6 -> html5)
    -> ({ formRecord | view : ctx -> valid -> List html6 } -> form)
    -> (Field valid a state msg error ctx localCtx html8 -> form -> continue)
    -> ctx 
    -> valid
    -> List html5
formView field form map1 map2 makeForm continue ctx val = 
    let fieldView = 
          field.view 
              (field.getContext ctx) 
              { state = field.state 
              , hints = field.hints 
              , label = field.label 
              , description = field.description
              , placeholder = field.placeholder
              }
              |> map1
                  (\ f -> 
                      updateField ctx val (Ok val) f form makeForm field continue
                  )
    in
    fieldView :: 
      (form.view ctx val
          |> List.map 
              (map2
                  (\ forms -> 
                      continue field forms 
                  )
              ) 
      )

append2 : 
   Field valid a state msg error context localCtx (Element (FieldMsg msg))
   -> ElementForm (a -> value) valid (a -> value) context 
   -> ElementForm value valid value context
append2 field (ElementForm form) = 
    ElementForm 
        { value = form.value field.state.value
        , validated =
            validateField (Ok field.state.value) form ElementForm field (append2 )
        , view = 
            formView field form Element.map Element.map ElementForm append2
        }


updateField :
    ctx 
    -> valid 
    -> Result error b 
    -> FieldMsg msg 
    -> formRecord
    -> (formRecord -> form)
    -> Field valid a state msg error ctx localCtx html
    -> (Field valid a state msg error ctx localCtx html -> form -> continue)
    -> continue
updateField ctx val validation msg form makeForm field continue =
    let check f =
            let hints = checkHints ctx val f 
            in
            case validation of 
                Ok _ -> continue { f | hints = hints } (makeForm form)
                Err e -> continue { f | hints = e :: hints } (makeForm form)
    in
    case msg of 
        FieldMsg fmsg -> 
            let state = field.update (field.getContext ctx) fmsg field.state
            in
                case field.hints of 
                    [] -> continue { field | state = state } (makeForm form)
                    _ -> check { field | state = state }

        Blur ->
            check field



appendMap : 
    Validation valid b error 
    -> Field valid a state msg error context localCtx (Html (FieldMsg msg)) 
    -> Form (a -> value) valid (b -> validated) context
    -> Form value valid validated context
appendMap validate field (Form form) = 
    Form 
        { value = form.value field.state.value
        , validated = \ ctx val ->
            validateField (validate val) form Form field (appendMap validate) ctx val
        , view = \ ctx val ->
              let fieldView = 
                    field.view 
                        (field.getContext ctx)
                        { state = field.state 
                        , hints = field.hints 
                        , label = field.label 
                        , description = field.description
                        , placeholder = field.placeholder
                        }
                        |> Html.map
                            (\ f -> 
                                updateField ctx val (validate val) f form Form field (appendMap validate)
                            )
              in
              fieldView :: 
                (form.view ctx val
                    |> List.map 
                        (Html.map
                            (\ forms -> 
                                appendMap validate field forms 
                            )
                        ) 
                )
        }



isValid : context -> Form value value validated context -> Result (Form value value validated context) validated
isValid context (Form form) =
    form.validated context form.value


view : context -> Form value value validated context-> List (Html (Form value value validated context))
view context (Form form) =
    form.view 
        context 
        form.value
        |> List.reverse


type alias ViewConfig value state hint = 
    { state : Value value state
    , hints : List hint
    , label : Maybe String
    , placeholder : Maybe String
    , description : Maybe String
    }


type alias Field form value state msg hint ctx fieldCtx view =
    { view : fieldCtx -> ViewConfig value state hint -> view
    , state : Value value state
    , update : fieldCtx -> msg -> Value value state -> Value value state 
    , hints : List hint
    , label : Maybe String
    , placeholder : Maybe String
    , description : Maybe String
    , getContext : ctx -> fieldCtx
    , hinting : List (Form.Valid.Hint hint value)
    , globalHinting : List (Form.Valid.GlobalHint ctx hint form)
    }


withLabel : String 
    -> Field form value state msg error ctx fieldCtx view 
    -> Field form value state msg error ctx fieldCtx view 
withLabel label field = 
    { field | label = Just label }

withPlaceholder : String 
    -> Field form value state msg error ctx fieldCtx view 
    -> Field form value state msg error ctx fieldCtx view 
withPlaceholder placeholder field = 
    { field | placeholder = Just placeholder }


withDescription : String 
    -> Field form value state msg error ctx fieldCtx view 
    -> Field form value state msg error ctx fieldCtx view 
withDescription description field = 
    { field | description = Just description }

withHints : List (Form.Valid.Hint hint value) 
    -> Field form value state msg hint ctx fieldCtx view 
    -> Field form value state msg hint ctx fieldCtx view
withHints hints field = 
    { field | hinting = hints }

withGlobalHints : List (Form.Valid.GlobalHint ctx hint form) 
    -> Field form value state msg hint ctx fieldCtx view 
    -> Field form value state msg hint ctx fieldCtx view
withGlobalHints hints field = 
    { field | globalHinting = hints }

