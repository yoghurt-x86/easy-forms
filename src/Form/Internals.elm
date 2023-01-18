module Form.Internals exposing (..)


import Form.Field exposing (Map, Field, FieldMsg(..))


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

