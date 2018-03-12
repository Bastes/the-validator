module TheValidator
    exposing
        ( Validation
        , Validator
        , all
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

@docs all, map, focus, focusMap, list

-}


{-| A `Validation` is a function that validates a model.
-}
type alias Validation model =
    model -> Bool


{-| A `Validator` holds validations and corresponding errors for a model.
The internals of this data type are not exposed, instead see the constructors
for more details.
-}
type Validator error model
    = Simple (model -> List error) (Validation model)
    | Composite (List (Validator error model))
    | Valid


{-| Checks wether a model is valid according to a validator. If you care about
the reasons why the model is invalid, consider using `validate` instead.

    bigNumber = simple (flip (>) 1000) "Not such a big number"

    isValid bigNumber 1001 == True
    isValid bigNumber 1000 == False

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


{-| Validate a model using a validator, returning a list of errors. The list
will be empty when the model is valid. If you only care about the validity of
the model, consider using the more efficient `isValid` instead.

    atLeast8Chars = simple (\s -> String.length s >= 8) "should be at least 8 chars long"

    validate atLeast8Chars "2short" == ["should be at least 8 chars long"]
    validate atLeast8Chars "long enough" == []

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


{-| A simple validator that returns an error constant.

    stayPositive = (simple (flip (>) 0) "only positive numbers are allowed here stranger")
    validate stayPositive 0 == ["only positive numbers are allowed here stranger"]

-}
simple : Validation model -> error -> Validator error model
simple isValid error =
    Simple (always [ error ]) isValid


{-| A parameterized validator that composes a single error from the model.

    justMonika = parameterized ((==) "Monika") (\name -> "Not " ++ name ++ "! Just Monika.")
    validate justMonika "Yuri" == ["Not Yuri! Just Monika."]

-}
parameterized : Validation model -> (model -> error) -> Validator error model
parameterized isValid error =
    Simple (error >> flip (::) []) isValid


{-| A validator that is always invalid. It will always provide the same error.

    validate (invalid "Bad, bad number!") 111 == ["Bad, bad number!"]
    isValid (invalid "Some error message") "blah" == True

-}
invalid : error -> Validator error model
invalid =
    simple (always False)


{-| A validator that is always valid. It will always pass, never provide error.

    validate valid 123 == []
    isValid valid "something" == True

-}
valid : Validator error model
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
all : List (Validator error model) -> Validator error model
all validators =
    Composite <| flattenAll validators


{-| Decorates a validator by modifying the errors it returns. The transformation
function handles the transformation from one error type to the other.

    onlyTrue = simple ((==) True) "This is simply not True!"
    onlyMoreTrue = onlyTrue |> map (\error -> ["No!", error, "It is False!"])

    validate onlyTrue False == ["This is simply not True!"]
    validate onlyMoreTrue False == ["No!", "This is simply not True!", "It is False!"]

-}
map : (model -> errorA -> errorB) -> Validator errorA model -> Validator errorB model
map transformation validator =
    case validator of
        Simple error isValid ->
            Simple (\item -> item |> error |> List.map (transformation item)) isValid

        Composite validators ->
            Composite <| List.map (map transformation) validators

        Valid ->
            Valid


{-| Creates a new validator to check another model. The transformation function
takes the model provided to the new validator, extracts the model expected by
the original validator and feeds it instead. Useful to focus on a field of a
record, part of a list, etc.

    type alias Fighter = { name: String, strength : Int }

    under9000 = simple (flip (<=) 9000) "It's over 9000!!!"
    onlyHuman = focus .strength under9000

    validate onlyHuman { name = "Satan", strength = 9 } == []
    validate onlyHuman { name = "Goku", strength = 999999 } == [ "It's over 9000!!!" ]

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


{-| Takes a validator and transform it to work on another model and change the
errors. Shortcut for `focus` then `map`.

    type alias User =
        { login : String, password : String }

    passwordValidator =
        simple ((/=) "password") "'password' is not a very good password"

    userValidator =
        focusMap .password (\error -> "for realz " ++ error) passwordValidator

    user = { name = "Carl Streator", password = "password" }

    validate userValidator == ["for realz 'password' is not a very good password"]

-}
focusMap : (modelB -> modelA) -> (errorA -> errorB) -> Validator errorA modelA -> Validator errorB modelB
focusMap modelTransformation errorTransformation =
    focus modelTransformation >> map (always errorTransformation)


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
      (\index model error -> (index, model.owner ++ "'s cup:", error))
      justRight

    validate goldilocks cups ==
      [ (1, "Mama Bear's cup:", "it's too hot!")
      , (2, "Papa Bear's cup:", "it's too cold!")
      ]

-}
list : (Int -> model -> errorA -> errorB) -> Validator errorA model -> Validator errorB (List model)
list transformation validator =
    case validator of
        Simple error isValid ->
            let
                indexedError index item =
                    validate (validator |> map (always (transformation index item))) item

                listError items =
                    items
                        |> List.indexedMap indexedError
                        |> List.concat
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
