module TheValidator
    exposing
        ( all
        , focus
        , isValid
        , map
        , parameterized
        , simple
        , validate
        )

{-| TODO

@docs validate, isValid

@docs simple, parameterized

@docs all

@docs map, focus

-}


type alias Validation model =
    model -> Bool


type Validator error model
    = Simple (model -> error) (Validation model)
    | Composite (List (Validator error model))


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


{-| validate
-}
validate : Validator error model -> model -> List error
validate validator model =
    case validator of
        Simple error isValid ->
            if model |> isValid then
                []
            else
                [ error model ]

        Composite validators ->
            validators
                |> List.concatMap (flip validate model)


{-| simple
-}
simple : Validation model -> error -> Validator error model
simple isValid error =
    Simple (always error) isValid


{-| parameterized
-}
parameterized : Validation model -> (model -> error) -> Validator error model
parameterized isValid error =
    Simple error isValid


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
            parameterized isValid (error >> transformation)

        Composite validators ->
            Composite <| List.map (map transformation) validators


{-| focus
-}
focus : (modelB -> modelA) -> Validator error modelA -> Validator error modelB
focus transformation validator =
    case validator of
        Simple error isValid ->
            parameterized
                (transformation >> isValid)
                (transformation >> error)

        Composite validators ->
            Composite <| List.map (focus transformation) validators



-- HELPERS


flatten : Validator error model -> List (Validator error model)
flatten validator =
    case validator of
        Composite validators ->
            flattenAll validators

        _ ->
            [ validator ]


flattenAll : List (Validator error model) -> List (Validator error model)
flattenAll =
    List.concatMap flatten
