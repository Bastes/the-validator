module TheValidator
    exposing
        ( Validation
        , Validator
        , all
        , focus
        , focusError
        , invalid
        , isValid
        , list
        , mapError
        , mapErrorWithModel
        , maybe
        , parameterized
        , simple
        , valid
        , validate
        )

{-| This library provides a way to express validations through Validator objects
holding the validation logic and allowing for composition.


# Types

@docs Validator

@docs Validation


# Validation

@docs validate, isValid


# Validator Constructors

@docs simple, parameterized, invalid, valid


# Validator Composition

@docs all, mapError, mapErrorWithModel, focus, focusError, maybe, list

-}


{-| A `Validation` is a function that validates a model.
-}
type alias Validation model =
    model -> Bool


{-| A `Validator` holds validations and corresponding errors for a model.
The internals of this data type are not exposed, instead see the constructors
for more details.
-}
type Validator model error
    = Simple (Validation model) (model -> List error)
    | Composite (List (Validator model error))
    | Valid


{-| Checks wether a model is valid according to a validator. If you care about
the reasons why the model is invalid, consider using `validate` instead.

    bigNumber = simple (flip (>) 1000) "Not such a big number"

    isValid bigNumber 1001 == True
    isValid bigNumber 1000 == False

-}
isValid : Validator model error -> model -> Bool
isValid validator model =
    case validator of
        Simple isValid error ->
            isValid model

        Composite validators ->
            validators
                |> List.map isValid
                |> List.all ((|>) model)

        Valid ->
            True


{-| Validate a model using a validator, returning a list of errors. The list
will be empty when the model is valid. If you only care about the validity of
the model, consider using the more efficient `isValid` instead.

    atLeast8Chars = simple (\s -> String.length s >= 8) "should be at least 8 chars long"

    validate atLeast8Chars "2short" == ["should be at least 8 chars long"]
    validate atLeast8Chars "long enough" == []

-}
validate : Validator model error -> model -> List error
validate validator model =
    case validator of
        Simple isValid error ->
            if model |> isValid then
                []
            else
                error model

        Composite validators ->
            validators
                |> List.concatMap (flip validate model)

        Valid ->
            []


{-| A simple validator that returns an error constant.

    stayPositive = (simple (flip (>) 0) "only positive numbers are allowed here stranger")
    validate stayPositive 0 == ["only positive numbers are allowed here stranger"]

-}
simple : Validation model -> error -> Validator model error
simple isValid error =
    Simple isValid (always [ error ])


{-| A parameterized validator that composes a single error from the model.

    justMonika = parameterized ((==) "Monika") (\name -> "Not " ++ name ++ "! Just Monika.")
    validate justMonika "Yuri" == ["Not Yuri! Just Monika."]

-}
parameterized : Validation model -> (model -> error) -> Validator model error
parameterized isValid error =
    Simple isValid (error >> flip (::) [])


{-| A validator that is always invalid. It will always provide the same error.

    validate (invalid "Bad, bad number!") 111 == ["Bad, bad number!"]
    isValid (invalid "Some error message") "blah" == True

-}
invalid : error -> Validator model error
invalid =
    simple (always False)


{-| A validator that is always valid. It will always pass, never provide error.

    validate valid 123 == []
    isValid valid "something" == True

-}
valid : Validator model error
valid =
    Valid



-- COMPOSITION


{-| Converts a list of validator into one new validator of the same type.
This is useful to aggregate multiple validations on the same model (fields in
an record, more than one check on a value, etc.).

    moreThan2 = simple (flip (>) 2) "must be greater than 2"
    notFactorOf3 = simple (\n -> n % 3 /= 0) "must not be a factor of 3"
    lessThan1000 = simple (flip (<) 1000) "must be lower than 1000"

    allThose =
      all
      [ moreThan2
      , notFactorOf3
      , lessThan1000
      ]

    validate allThose 1 == ["must be greater than 2"]
    validate allThose 333 == ["must not be a factor of 3"]
    validate allThose 1000 == ["must be lower than 1000"]
    validate allThose 12345 == ["must not be a factor of 3", "must be lower than 1000"]
    validate allThose 124 == []

    isValid allThose 778 == True
    isValid allTHose 12 == False

-}
all : List (Validator model error) -> Validator model error
all validators =
    Composite <| flattenAll validators


{-| Decorates a validator by modifying the errors it returns. The transformation
function handles the transformation from one error type to the other.

(fans of functional programming will recognize profunctor's rmap)

    onlyTrue = simple ((==) True) "This is simply not True!"
    onlyMoreTrue = onlyTrue |> mapError (\error -> (error, "It is simply False!"))

    validate onlyTrue False == ["This is simply not True!"]
    validate onlyMoreTrue False == ("This is simply not True!", "It is simply False!")

-}
mapError : (errorA -> errorB) -> Validator model errorA -> Validator model errorB
mapError transformation validator =
    case validator of
        Simple isValid error ->
            Simple isValid (error >> List.map transformation)

        Composite validators ->
            Composite <| List.map (mapError transformation) validators

        Valid ->
            Valid


{-| Same as mapError but with the model as an additional firt parameter to the
iterator function.

    onlyTrue = simple ((==) True) "This is simply not True!"
    onlyMoreTrue = onlyTrue |> mapErrorWithModel (\model error -> (error, "It is simply " ++ toString model ++ !"))

    validate onlyTrue False == ["This is simply not True!"]
    validate onlyMoreTrue False == ("This is simply not True!", "It is simply False!")

-}
mapErrorWithModel : (model -> errorA -> errorB) -> Validator model errorA -> Validator model errorB
mapErrorWithModel transformation validator =
    case validator of
        Simple isValid error ->
            Simple isValid (\model -> model |> error |> List.map (transformation model))

        Composite validators ->
            Composite <| List.map (mapErrorWithModel transformation) validators

        Valid ->
            Valid


{-| Creates a new validator to check another model. The transformation function
takes the model provided to the new validator, extracts the model expected by
the original validator and feeds it instead. Useful to focus on a field of a
record, part of a list, etc.

(fans of functional programming will recognize profunctor's lmap)

    type alias Fighter = { name: String, strength : Int }

    under9000 = simple (flip (<=) 9000) "It's over 9000!!!"
    onlyHuman = focus .strength under9000

    validate onlyHuman { name = "Satan", strength = 9 } == []
    validate onlyHuman { name = "Goku", strength = 999999 } == [ "It's over 9000!!!" ]

-}
focus : (modelB -> modelA) -> Validator modelA error -> Validator modelB error
focus transformation validator =
    case validator of
        Simple isValid error ->
            Simple
                (transformation >> isValid)
                (transformation >> error)

        Composite validators ->
            Composite <| List.map (focus transformation) validators

        Valid ->
            Valid


{-| Takes a validator and transform it to work on another model and change the
errors. Shortcut for `focus` then `mapError`.

(fans of functional programming will recognize profunctor's dimap)

    type alias User =
        { login : String, password : String }

    passwordValidator =
        simple ((/=) "password") "'password' is not a very good password"

    userValidator =
        focusError .password (\{login} error -> "for realz, " ++ login ++ " " ++ error) passwordValidator

    user = { name = "Carl Streator", password = "password" }

    validate userValidator user == ["for realz Carl Streator 'password' is not a very good password"]

-}
focusError :
    (modelB -> modelA)
    -> (errorA -> errorB)
    -> Validator modelA errorA
    -> Validator modelB errorB
focusError modelTransformation errorTransformation =
    focus modelTransformation >> mapError errorTransformation


{-| Focuses on a value that may or may not be available for validation.
When there is nothing, the validation succeeds by default.

    notBlank = simple ((/=) "") "filled or nothing"
    nameNotBlank = maybe .name notBlank

    isValid notBlank { name = Nothing } == True
    validate notBlank { name = Nothing } == []
    isValid notBlank { name = Just "Someone" } == True
    validate notBlank { name = Just "Me" } == []
    isValid notBlank { name = Just "" } == False
    validate notBlank { name = Just "" } == ["filled or nothing"]

-}
maybe : (modelB -> Maybe modelA) -> Validator modelA error -> Validator modelB error
maybe transformation validator =
    case validator of
        Simple isValid error ->
            Simple
                (transformation >> Maybe.map isValid >> Maybe.withDefault True)
                (transformation >> Maybe.map error >> Maybe.withDefault [])

        Composite validators ->
            Composite <| List.map (maybe transformation) validators

        Valid ->
            Valid


{-| Makes a validator that applies another validator to a list of elements.
The transformation function receives the index of the element under scrutiny as
well as the error obtained by the internal validator.

    model Cup = {owner: String, temperature: Int}
    cups =
      [ {owner: "Mama Bear", temperature: 100}
      , {owner: "Papa Bear", temperature: 20}
      , {owner: "Little Bear", temperature: 30}
      ]

    tooHot = simple (\cup -> cup.temperature > 30) "it's too hot!"
    tooCold = simple (\cup -> cup.temperature < 30) "it's too cold!"

    justRight = all [tooHot, tooCold]

    goldilocks = list
      (\index model error -> (index, error))
      justRight

    validate goldilocks cups ==
      [ (1, "it's too hot!")
      , (2, "it's too cold!")
      ]

-}
list :
    (Int -> errorA -> errorB)
    -> Validator model errorA
    -> Validator (List model) errorB
list transformation validator =
    case validator of
        Simple isValid error ->
            let
                indexedError index =
                    validate (validator |> mapError (transformation index))

                listError items =
                    items
                        |> List.indexedMap indexedError
                        |> List.concat
            in
            Simple (List.all isValid) listError

        Composite validators ->
            Composite (validators |> List.map (list transformation))

        Valid ->
            Valid



-- HELPERS


flatten : Validator model error -> List (Validator model error)
flatten validator =
    case validator of
        Composite validators ->
            flattenAll validators

        Valid ->
            []

        _ ->
            [ validator ]


flattenAll : List (Validator model error) -> List (Validator model error)
flattenAll =
    List.concatMap flatten
