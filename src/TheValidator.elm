module TheValidator
    exposing
        ( all
        , focus
        , focusMap
        , invalid
        , isValid
        , list
        , map
        , parameterized
        , simple
        , valid
        , validate
        )

{-| TODO

@docs validate, isValid

@docs simple, parameterized, invalid, valid

@docs all

@docs map, focus, focusMap, list

-}


type alias Validation model =
    model -> Bool


type Validator error model
    = Simple (model -> List error) (Validation model)
    | Composite (List (Validator error model))
    | Valid


{-| isValid
-}
isValid : Validator error model -> model -> Bool
isValid validator model =
    case validator of
        Simple error isValid ->
            isValid model

        Composite validators ->
            validators
                |> List.map isValid
                |> List.all ((|>) model)

        Valid ->
            True


{-| validate
-}
validate : Validator error model -> model -> List error
validate validator model =
    case validator of
        Simple error isValid ->
            if model |> isValid then
                []
            else
                error model

        Composite validators ->
            validators
                |> List.concatMap (flip validate model)

        Valid ->
            []


{-| simple
-}
simple : Validation model -> error -> Validator error model
simple isValid error =
    Simple (always [ error ]) isValid


{-| parameterized
-}
parameterized : Validation model -> (model -> error) -> Validator error model
parameterized isValid error =
    Simple (error >> flip (::) []) isValid


{-| invalid
-}
invalid : error -> Validator error model
invalid =
    simple (always False)


{-| valid
-}
valid : Validator error model
valid =
    Valid


{-| all
-}
all : List (Validator error model) -> Validator error model
all validators =
    Composite <| flattenAll validators


{-| map
-}
map : (errorA -> errorB) -> Validator errorA model -> Validator errorB model
map transformation validator =
    case validator of
        Simple error isValid ->
            Simple (error >> List.map transformation) isValid

        Composite validators ->
            Composite <| List.map (map transformation) validators

        Valid ->
            Valid


{-| focus
-}
focus : (modelB -> modelA) -> Validator error modelA -> Validator error modelB
focus transformation validator =
    case validator of
        Simple error isValid ->
            Simple
                (transformation >> error)
                (transformation >> isValid)

        Composite validators ->
            Composite <| List.map (focus transformation) validators

        Valid ->
            Valid


{-| focusMap
-}
focusMap : (modelB -> modelA) -> (errorA -> errorB) -> Validator errorA modelA -> Validator errorB modelB
focusMap modelTransformation errorTransformation =
    focus modelTransformation >> map errorTransformation


{-| list
-}
list : (Int -> errorA -> errorB) -> Validator errorA model -> Validator errorB (List model)
list transformation validator =
    case validator of
        Simple error isValid ->
            let
                indexedError index =
                    validate (validator |> map (transformation index))

                listError =
                    List.indexedMap indexedError
                        >> List.concat
            in
            Simple listError (List.all isValid)

        Composite validators ->
            Composite (validators |> List.map (list transformation))

        Valid ->
            Valid



-- HELPERS


flatten : Validator error model -> List (Validator error model)
flatten validator =
    case validator of
        Composite validators ->
            flattenAll validators

        Valid ->
            []

        _ ->
            [ validator ]


flattenAll : List (Validator error model) -> List (Validator error model)
flattenAll =
    List.concatMap flatten
