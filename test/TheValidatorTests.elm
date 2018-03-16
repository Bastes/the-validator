module TheValidatorTests exposing (theValidatorTests)

import Expect
import Fuzz exposing (..)
import Test exposing (..)
import TheValidator as Validator exposing (..)


theValidatorTests : Test
theValidatorTests =
    describe "TheValidator"
        [ describe "simple" <|
            let
                aValidator =
                    Validator.simple
                        ((/=) "")
                        "Hey, this string is blank! WTF dude?"
            in
            [ test "rejects invalid models with the error" <|
                \_ ->
                    aValidator
                        |> validating ""
                        |> expectAnError "Hey, this string is blank! WTF dude?"
            , fuzz aNonEmptyString "accepts a valid model" <|
                \anything ->
                    aValidator
                        |> validating anything
                        |> expectValidity
            ]
        , describe "parameterized" <|
            let
                aValidator =
                    Validator.parameterized
                        (flip (<) 3)
                        (\n -> toString n ++ " should be lower than 3!")
            in
            [ fuzz (intRange 3 1000) "reports the error, using the model's value" <|
                \n ->
                    aValidator
                        |> validating n
                        |> expectAnError (toString n ++ " should be lower than 3!")
            , fuzz (intRange -1000 2) "validates a valid model" <|
                \n ->
                    aValidator
                        |> validating n
                        |> expectValidity
            ]
        , describe "invalid" <|
            [ fuzz int "it always reports a failure, no matter what the input" <|
                \n ->
                    Validator.invalid "this is always invalid"
                        |> validating n
                        |> expectAnError "this is always invalid"
            ]
        , describe "valid" <|
            [ fuzz int "it never reports a failure, no matter what the input" <|
                \n ->
                    Validator.valid
                        |> validating n
                        |> expectValidity
            ]
        , describe "all" <|
            let
                notFizz =
                    Validator.simple
                        (\n -> n % 3 /= 0)
                        "not fizz!"

                notBuzz =
                    Validator.simple
                        (\n -> n % 5 /= 0)
                        "not buzz!"

                notFizzBuzz =
                    Validator.simple
                        (\n -> n % 15 /= 0)
                        "not fizzbuzz!"

                noneOfTheseFizzAndBuzzes =
                    Validator.all
                        [ notFizz
                        , notBuzz
                        , notFizzBuzz
                        ]
            in
            [ fuzz aFizzBuzzInt "it reports all errors when all are triggered" <|
                \n ->
                    noneOfTheseFizzAndBuzzes
                        |> validating n
                        |> expectErrors [ "not fizz!", "not buzz!", "not fizzbuzz!" ]
            , fuzz aBuzzOnlyInt "it reports only the subset of errors that are triggered" <|
                \n ->
                    noneOfTheseFizzAndBuzzes
                        |> validating n
                        |> expectAnError "not buzz!"
            , fuzz aNonFizzBuzzInt "it reports nothing when there's nothing to report" <|
                \n ->
                    noneOfTheseFizzAndBuzzes
                        |> validating n
                        |> expectValidity
            ]
        , describe "map" <|
            let
                positive =
                    Validator.simple
                        (flip (>) 0)
                        "needs to be positive"

                notFactorOf3 =
                    Validator.parameterized
                        (\n -> (n % 3) /= 0)
                        (\n -> toString n ++ " needs not be a factor of 3")

                positiveAndNotFactorOf3 =
                    Validator.all
                        [ positive
                        , notFactorOf3
                        ]
                        |> Validator.map (\n error -> ( "the number", n, error ))
            in
            [ fuzz aFactorOf3 "it maps over all errors" <|
                \n ->
                    positiveAndNotFactorOf3
                        |> validating n
                        |> expectErrors
                            [ ( "the number", n, "needs to be positive" )
                            , ( "the number", n, toString n ++ " needs not be a factor of 3" )
                            ]
            , fuzz aNonFactorOf3 "it does not report errors when there are none" <|
                \n ->
                    positiveAndNotFactorOf3
                        |> validating n
                        |> expectValidity
            ]
        , describe "focus, focusMap" <|
            let
                names =
                    [ "Oyuki-Chan", "David Mills", "Mr. Tulip", "Debra Morgan", "Sandor Clegane" ]

                naughtyNicks =
                    [ "bugger", "jay", "dunderhead", "fribble", "gadabout" ]

                aNonNaughtyNick =
                    conditional
                        { retries = 10
                        , fallback = always "gentleman"
                        , condition = \n -> List.member n naughtyNicks
                        }
                        string

                aNaughtyCall =
                    Fuzz.tuple ( oneOfThese names, oneOfThese naughtyNicks )
                        |> Fuzz.map
                            (\( name, isCallingYou ) ->
                                { name = name
                                , isCallingYou = isCallingYou
                                }
                            )

                aNonNaughtyCall =
                    Fuzz.tuple ( oneOfThese names, aNonNaughtyNick )
                        |> Fuzz.map
                            (\( name, isCallingYou ) ->
                                { name = name
                                , isCallingYou = isCallingYou
                                }
                            )

                polite =
                    Validator.parameterized
                        (not << flip List.member naughtyNicks)
                        (\isCallingYou -> [ "please refrain from calling me a", isCallingYou ])
            in
            [ describe "focus" <|
                let
                    politesse =
                        Validator.focus .isCallingYou polite
                in
                [ fuzz aNaughtyCall "it shows the errors on the detail" <|
                    \call ->
                        politesse
                            |> validating call
                            |> expectAnError
                                [ "please refrain from calling me a", call.isCallingYou ]
                , fuzz aNonNaughtyCall "it shows no error when there is none" <|
                    \call ->
                        politesse
                            |> validating call
                            |> expectValidity
                ]
            , describe "focusMap" <|
                let
                    politesse =
                        Validator.focusMap
                            .isCallingYou
                            (\{ name } error -> [ name, "dear" ] ++ error ++ [ "will you?" ])
                            polite
                in
                [ fuzz aNaughtyCall "it shows the wrapped errors on the detail" <|
                    \call ->
                        politesse
                            |> validating call
                            |> expectAnError
                                [ call.name, "dear", "please refrain from calling me a", call.isCallingYou, "will you?" ]
                , fuzz aNonNaughtyCall "it shows no error when there is none" <|
                    \call ->
                        politesse
                            |> validating call
                            |> expectValidity
                ]
            ]
        , describe "maybe" <|
            [ describe "with a simple validator" <|
                let
                    maybeValidator =
                        simple ((/=) "") "all I know is I should know Nothing or Just something instead"
                            |> Validator.maybe .knows
                in
                [ test "always pass when there is nothing" <|
                    \_ ->
                        maybeValidator
                            |> validating { name = "Jon Snow", knows = Nothing }
                            |> expectValidity
                , test "fails when there is something that fails" <|
                    \_ ->
                        maybeValidator
                            |> validating { name = "Socrates", knows = Just "" }
                            |> expectAnError "all I know is I should know Nothing or Just something instead"
                , fuzz aNonEmptyString "succeeds when there is something that succeeds" <|
                    \anything ->
                        maybeValidator
                            |> validating { name = "Dr. Manhattan", knows = Just anything }
                            |> expectValidity
                ]
            , describe "with a composite validator" <|
                let
                    maybeCompositeValidator =
                        all
                            [ simple ((/=) 4) "four shalt thou not count"
                            , simple ((/=) 2) "nor either count thou two"
                            , simple ((/=) 5) "five is right out"
                            ]
                            |> Validator.maybe .counts
                in
                [ test "always pass when there is nothing" <|
                    \_ ->
                        maybeCompositeValidator
                            |> validating { name = "Maynard", counts = Nothing }
                            |> expectValidity
                , test "fails when there is something that fails" <|
                    \_ ->
                        maybeCompositeValidator
                            |> validating { name = "Arthur", counts = Just 5 }
                            |> expectAnError "five is right out"
                , fuzz (intRange 6 1000) "succeeds when there is something that succeeds" <|
                    \theCount ->
                        maybeCompositeValidator
                            |> validating { name = "Galahad", counts = Just theCount }
                            |> expectValidity
                ]
            ]
        , describe "list" <|
            let
                positive =
                    Validator.simple (flip (>) 0) "is not positive"

                allPositive =
                    Validator.list (\index model error -> ( index, model, error )) positive
            in
            [ fuzz (aListOfAtLeastOne (intRange -1000 0)) "it shows errors on each invalid items" <|
                \elements ->
                    allPositive
                        |> validating elements
                        |> expectErrors
                            (List.indexedMap
                                (\index model -> ( index, model, "is not positive" ))
                                elements
                            )
            , fuzz
                (Fuzz.tuple3
                    ( Fuzz.list (intRange 1 1000)
                    , intRange -1000 0
                    , Fuzz.list (intRange 1 1000)
                    )
                )
                "it shows errors only on invalid items"
              <|
                \( before, bad, after ) ->
                    allPositive
                        |> validating (before ++ [ bad ] ++ after)
                        |> expectAnError ( List.length before, bad, "is not positive" )
            , fuzz (Fuzz.list (intRange 1 1000)) "it shows no error on valid items" <|
                \elements ->
                    allPositive
                        |> validating elements
                        |> expectValidity
            ]
        ]



-- HELPERS


applyBoth : ( a -> b, a -> c ) -> a -> ( b, c )
applyBoth ( aToB, aToC ) a =
    ( aToB a, aToC a )


validating : model -> Validator error model -> ( Bool, List error )
validating model validator =
    applyBoth (applyBoth ( isValid, validate ) validator) model


expectValidity : ( Bool, List error ) -> Expect.Expectation
expectValidity =
    Expect.equal ( True, [] )


expectAnError : error -> ( Bool, List error ) -> Expect.Expectation
expectAnError error =
    expectErrors [ error ]


expectErrors : List error -> ( Bool, List error ) -> Expect.Expectation
expectErrors errors =
    Expect.equal ( False, errors )


oneOfThese : List a -> Fuzzer a
oneOfThese =
    oneOf << List.map constant


aFizzBuzzInt : Fuzzer Int
aFizzBuzzInt =
    intRange 1 1000 |> Fuzz.map ((*) 15)


aListOfAtLeastOne : Fuzzer a -> Fuzzer (List a)
aListOfAtLeastOne =
    conditional
        { retries = 10
        , fallback = always []
        , condition = not << List.isEmpty
        }
        << Fuzz.list


aNonFizzBuzzInt : Fuzzer Int
aNonFizzBuzzInt =
    conditional
        { retries = 10
        , fallback = always 7
        , condition = \n -> (n % 3 /= 0) && (n % 5 /= 0)
        }
        (intRange 1 1000)


aBuzzOnlyInt : Fuzzer Int
aBuzzOnlyInt =
    aNonFactorOf3 |> Fuzz.map ((*) 5)


aFactorOf3 : Fuzzer Int
aFactorOf3 =
    intRange -1000 0 |> Fuzz.map ((*) 3)


aNonFactorOf3 : Fuzzer Int
aNonFactorOf3 =
    conditional
        { retries = 10
        , fallback = always 7
        , condition = \n -> (n % 3) /= 0
        }
        (intRange 1 1000)


aNonEmptyString : Fuzzer String
aNonEmptyString =
    conditional
        { retries = 10
        , fallback = always "time"
        , condition = (/=) ""
        }
        string
