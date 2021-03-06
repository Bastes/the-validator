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
                        (\n -> n < 3)
                        (\n -> String.fromInt n ++ " should be lower than 3!")
            in
            [ fuzz (intRange 3 1000) "reports the error, using the model's value" <|
                \n ->
                    aValidator
                        |> validating n
                        |> expectAnError (String.fromInt n ++ " should be lower than 3!")
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
                        (\n -> (n |> modBy 3) /= 0)
                        "not fizz!"

                notBuzz =
                    Validator.simple
                        (\n -> (n |> modBy 5) /= 0)
                        "not buzz!"

                notFizzBuzz =
                    Validator.simple
                        (\n -> (n |> modBy 15) /= 0)
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
        , describe "mapError, mapErrorWithModel" <|
            let
                positive =
                    Validator.simple
                        (\n -> n > 0)
                        "needs to be positive"

                notFactorOf3 =
                    Validator.parameterized
                        (\n -> (n |> modBy 3) /= 0)
                        (\n -> String.fromInt n ++ " needs not be a factor of 3")

                not5Aces =
                    Validator.focusInside
                        (\{ rank, count } ->
                            if rank == "Ace" then
                                Validator.focus
                                    .count
                                    (Validator.simple (\n -> n < 5) "5 or more is suspect for aces")

                            else
                                valid
                        )
            in
            [ describe "mapError" <|
                let
                    positiveAndNotFactorOf3 =
                        Validator.all
                            [ focus .count positive
                            , focus .count notFactorOf3
                            , not5Aces
                            ]
                            |> Validator.mapError (\error -> ( "the number", error ))
                in
                [ fuzz aFactorOf3 "it maps over all errors" <|
                    \n ->
                        positiveAndNotFactorOf3
                            |> validating { rank = "Jack", count = n }
                            |> expectErrors
                                [ ( "the number", "needs to be positive" )
                                , ( "the number", String.fromInt n ++ " needs not be a factor of 3" )
                                ]
                , test "it also maps over generated validators" <|
                    \_ ->
                        positiveAndNotFactorOf3
                            |> validating { rank = "Ace", count = 5 }
                            |> expectErrors
                                [ ( "the number", "5 or more is suspect for aces" )
                                ]
                , fuzz aPositiveNonFactorOf3 "it does not report errors when there are none" <|
                    \n ->
                        positiveAndNotFactorOf3
                            |> validating { rank = "Jack", count = n }
                            |> expectValidity
                ]
            , describe "mapErrorWithModel" <|
                let
                    positiveAndNotFactorOf3 =
                        Validator.all
                            [ focus .count positive
                            , focus .count notFactorOf3
                            , not5Aces
                            ]
                            |> Validator.mapErrorWithModel (\model error -> ( ( "model:", model ), ( "error:", error ) ))
                in
                [ fuzz aFactorOf3 "it maps over all errors" <|
                    \n ->
                        let
                            model =
                                { rank = "Queen", count = n }
                        in
                        positiveAndNotFactorOf3
                            |> validating model
                            |> expectErrors
                                [ ( ( "model:", model ), ( "error:", "needs to be positive" ) )
                                , ( ( "model:", model ), ( "error:", String.fromInt n ++ " needs not be a factor of 3" ) )
                                ]
                , test "it also maps over generated validators" <|
                    \_ ->
                        let
                            model =
                                { rank = "Ace", count = 5 }
                        in
                        positiveAndNotFactorOf3
                            |> validating model
                            |> expectErrors
                                [ ( ( "model:", model ), ( "error:", "5 or more is suspect for aces" ) )
                                ]
                , fuzz aPositiveNonFactorOf3 "it does not report errors when there are none" <|
                    \n ->
                        positiveAndNotFactorOf3
                            |> validating { rank = "Queen", count = n }
                            |> expectValidity
                ]
            ]
        , describe "focus, focusError" <|
            let
                names =
                    [ "Oyuki-Chan", "David Mills", "Mr. Tulip", "Debra Morgan", "Sandor Clegane" ]

                naughtyNicks =
                    [ "bugger", "jay", "dunderhead", "fribble", "gadabout" ]

                aNonNaughtyNick =
                    Fuzz.oneOf
                        ([ "dude", "friend", "sir", "pal", "mister" ]
                            |> List.map Fuzz.constant
                        )

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
                        (not << (\nick -> List.member nick naughtyNicks))
                        (\isCallingYou -> [ "please refrain from calling me a", isCallingYou ])

                smart =
                    Validator.all
                        [ simple (.luck >> (\n -> n > 5)) "is quite unlucky"
                        , simple (.intelligence >> (\n -> n > 5)) "is pretty dumb"
                        ]

                gifted =
                    Validator.focusInside
                        (\{ luck, intelligence } ->
                            if (luck + intelligence) > 10 then
                                Validator.valid

                            else
                                Validator.invalid "is not gifted"
                        )

                personFromTuple ( name, luck, intelligence ) =
                    { name = name
                    , special =
                        { luck = luck
                        , intelligence = intelligence
                        }
                    }

                aDumbLuckyGuy =
                    Fuzz.tuple3 ( string, intRange 6 10, intRange 1 5 )
                        |> Fuzz.map personFromTuple

                aDumbUnluckyGuy =
                    Fuzz.tuple3 ( string, intRange 1 5, intRange 1 5 )
                        |> Fuzz.map personFromTuple

                aSmartGuy =
                    Fuzz.tuple3 ( string, intRange 6 10, intRange 6 10 )
                        |> Fuzz.map personFromTuple
            in
            [ describe "focus"
                [ describe "with a single original validator" <|
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
                , describe "with a composite original validator" <|
                    let
                        smartGuy =
                            focus .special smart
                    in
                    [ fuzz aDumbUnluckyGuy "it shows all errors on the detail" <|
                        \lucklessDummy ->
                            smartGuy
                                |> validating lucklessDummy
                                |> expectErrors
                                    [ "is quite unlucky"
                                    , "is pretty dumb"
                                    ]
                    , fuzz aDumbLuckyGuy "it shows the errors on the detail" <|
                        \dummy ->
                            smartGuy
                                |> validating dummy
                                |> expectAnError
                                    "is pretty dumb"
                    , fuzz aSmartGuy "it shows no error when there is none" <|
                        \guy ->
                            smartGuy
                                |> validating guy
                                |> expectValidity
                    ]
                , describe "with a generated validator" <|
                    let
                        giftedGuy =
                            focus .special gifted
                    in
                    [ test "it shows errors on the detail when there are any" <|
                        \_ ->
                            giftedGuy
                                |> validating { name = "Rantamplan", special = { luck = 8, intelligence = 1 } }
                                |> expectErrors
                                    [ "is not gifted"
                                    ]
                    , test "it shows no error when there is none" <|
                        \_ ->
                            giftedGuy
                                |> validating { name = "Lucky Luke", special = { luck = 10, intelligence = 7 } }
                                |> expectValidity
                    ]
                ]
            , describe "focusError"
                [ describe "with a simple validator" <|
                    let
                        politesse =
                            Validator.focusError
                                .isCallingYou
                                (\error -> error ++ [ "will you?" ])
                                polite
                    in
                    [ fuzz aNaughtyCall "it shows the wrapped errors on the detail" <|
                        \call ->
                            politesse
                                |> validating call
                                |> expectAnError
                                    [ "please refrain from calling me a", call.isCallingYou, "will you?" ]
                    , fuzz aNonNaughtyCall "it shows no error when there is none" <|
                        \call ->
                            politesse
                                |> validating call
                                |> expectValidity
                    ]
                , describe "with a composite validator" <|
                    let
                        smartGuy =
                            Validator.focusError
                                .special
                                (\error -> [ "that guy", error ])
                                smart
                    in
                    [ fuzz aDumbUnluckyGuy "it shows all errors on the detail" <|
                        \lucklessDummy ->
                            smartGuy
                                |> validating lucklessDummy
                                |> expectErrors
                                    [ [ "that guy", "is quite unlucky" ]
                                    , [ "that guy", "is pretty dumb" ]
                                    ]
                    , fuzz aDumbLuckyGuy "it shows the errors on the detail" <|
                        \dummy ->
                            smartGuy
                                |> validating dummy
                                |> expectAnError
                                    [ "that guy", "is pretty dumb" ]
                    , fuzz aSmartGuy "it shows no error when there is none" <|
                        \guy ->
                            smartGuy
                                |> validating guy
                                |> expectValidity
                    ]
                , describe "with a generated validator" <|
                    let
                        giftedGuy =
                            Validator.focusError
                                .special
                                (\error -> [ "that guy", error ])
                                gifted
                    in
                    [ test "it shows errors on the detail when there are any" <|
                        \_ ->
                            giftedGuy
                                |> validating { name = "Rantamplan", special = { luck = 8, intelligence = 1 } }
                                |> expectErrors
                                    [ [ "that guy", "is not gifted" ]
                                    ]
                    , test "it shows no error when there is none" <|
                        \_ ->
                            giftedGuy
                                |> validating { name = "Lucky Luke", special = { luck = 10, intelligence = 7 } }
                                |> expectValidity
                    ]
                ]
            ]
        , describe "focusInside" <|
            let
                boundaries =
                    Validator.focusInside
                        (\{ min, max } ->
                            all
                                [ focus .min (Validator.simple (\n -> n < max) "the min should be less than the max")
                                , focus .max (Validator.simple (\n -> n > min) "the max should be more than the min")
                                ]
                        )
            in
            [ test "the validator produced rejects what is invalid" <|
                \_ ->
                    boundaries
                        |> validating { min = 1, max = 1 }
                        |> expectErrors
                            [ "the min should be less than the max"
                            , "the max should be more than the min"
                            ]
            , test "the validator produced validates what is valid" <|
                \_ ->
                    boundaries
                        |> validating { min = 1, max = 2 }
                        |> expectValidity
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
            , describe "with a generated validator" <|
                let
                    maybeGeneratedValidator =
                        focusInside
                            (\n1 ->
                                if (n1 |> modBy 3) == 0 then
                                    simple (\n2 -> n2 > 123) "factors of 3 should be more than 123"

                                else if (n1 |> modBy 2) == 0 then
                                    simple (\n2 -> n2 < 123) "non-factors of 3 factors of 2 should be less than 123"

                                else
                                    valid
                            )
                            |> Validator.maybe .counts
                in
                [ test "always pass when there is nothing" <|
                    \_ ->
                        maybeGeneratedValidator
                            |> validating { name = "Lancelot", counts = Nothing }
                            |> expectValidity
                , test "fails when there is something that fails" <|
                    \_ ->
                        maybeGeneratedValidator
                            |> validating { name = "Sir Robin's minstrel", counts = Just 124 }
                            |> expectAnError "non-factors of 3 factors of 2 should be less than 123"
                , test "succeeds when there is something that succeeds" <|
                    \_ ->
                        maybeGeneratedValidator
                            |> validating { name = "Sir Robin", counts = Just 234 }
                            |> expectValidity
                ]
            ]
        , describe "list"
            [ describe "with a simple validator" <|
                let
                    positive =
                        Validator.simple (\n -> n > 0) "is not positive"

                    allPositive =
                        Validator.list
                            (\index error -> ( index, error ))
                            positive
                in
                [ fuzz (aListOfAtLeastOne (intRange -1000 0)) "it shows errors on each invalid items" <|
                    \elements ->
                        allPositive
                            |> validating elements
                            |> expectErrors
                                (List.indexedMap
                                    (\index _ -> ( index, "is not positive" ))
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
                            |> expectAnError ( List.length before, "is not positive" )
                , fuzz (Fuzz.list (intRange 1 1000)) "it shows no error on valid items" <|
                    \elements ->
                        allPositive
                            |> validating elements
                            |> expectValidity
                ]
            , describe "with a composite validator" <|
                let
                    positive =
                        Validator.simple (\n -> n > 0) "is not positive"

                    under100 =
                        Validator.simple (\n -> n < 100) "is over 99"

                    positiveUnder100 =
                        Validator.all
                            [ positive
                            , under100
                            ]

                    allPositiveUnder100 =
                        Validator.list
                            (\index error -> ( index, error ))
                            positiveUnder100
                in
                [ fuzz (aListOfAtLeastOne (intRange -1000 0)) "it shows errors on each invalid items" <|
                    \elements ->
                        allPositiveUnder100
                            |> validating elements
                            |> expectErrors
                                (List.indexedMap
                                    (\index _ -> ( index, "is not positive" ))
                                    elements
                                )
                , fuzz
                    (Fuzz.tuple
                        ( Fuzz.tuple
                            ( Fuzz.list (intRange 1 99)
                            , Fuzz.list (intRange 1 99)
                            )
                        , Fuzz.tuple
                            ( intRange -1000 0
                            , intRange 100 1000
                            )
                        )
                    )
                    "it shows errors only on invalid items"
                  <|
                    \( ( before, after ), ( bad1, bad2 ) ) ->
                        allPositiveUnder100
                            |> validating (before ++ [ bad1, bad2 ] ++ after)
                            |> expectErrors
                                [ ( List.length before, "is not positive" )
                                , ( List.length before + 1, "is over 99" )
                                ]
                , fuzz (Fuzz.list (intRange 1 99)) "it shows no error on valid items" <|
                    \elements ->
                        allPositiveUnder100
                            |> validating elements
                            |> expectValidity
                ]
            , describe "with a generated validator" <|
                let
                    positiveOver100 =
                        Validator.focusInside
                            (\n1 ->
                                if n1 > 0 then
                                    simple (\n2 -> n2 > 100) "should be more than 100 when positive"

                                else
                                    valid
                            )

                    allPositiveOver100 =
                        Validator.list
                            (\index error -> ( index, error ))
                            positiveOver100
                in
                [ fuzz (aListOfAtLeastOne (intRange 1 100)) "it shows errors on each invalid items" <|
                    \elements ->
                        allPositiveOver100
                            |> validating elements
                            |> expectErrors
                                (List.indexedMap
                                    (\index _ -> ( index, "should be more than 100 when positive" ))
                                    elements
                                )
                , fuzz
                    (Fuzz.tuple3
                        ( Fuzz.list (intRange 101 1000)
                        , intRange 1 100
                        , Fuzz.list (intRange -1000 0)
                        )
                    )
                    "it shows errors only on invalid items"
                  <|
                    \( before, bad, after ) ->
                        allPositiveOver100
                            |> validating (before ++ [ bad ] ++ after)
                            |> expectErrors
                                [ ( List.length before, "should be more than 100 when positive" )
                                ]
                , fuzz (Fuzz.list (intRange 101 1000)) "it shows no error on valid items" <|
                    \elements ->
                        allPositiveOver100
                            |> validating elements
                            |> expectValidity
                ]
            ]
        ]



-- HELPERS


applyBoth : ( a -> b, a -> c ) -> a -> ( b, c )
applyBoth ( aToB, aToC ) a =
    ( aToB a, aToC a )


validating : model -> Validator model error -> ( Bool, List error )
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
aListOfAtLeastOne fuzzer =
    Fuzz.tuple ( fuzzer, Fuzz.list fuzzer )
        |> Fuzz.map (\( item, list ) -> item :: list)


aNonFizzBuzzInt : Fuzzer Int
aNonFizzBuzzInt =
    let
        isFizzOrBuzz n =
            ((n |> modBy 3) == 0) || ((n |> modBy 5) == 0)

        makeNonFizzBuzz n =
            if isFizzOrBuzz n then
                makeNonFizzBuzz (n + 1)

            else
                n
    in
    Fuzz.int
        |> Fuzz.map makeNonFizzBuzz


aBuzzOnlyInt : Fuzzer Int
aBuzzOnlyInt =
    aPositiveNonFactorOf3 |> Fuzz.map ((*) 5)


aFactorOf3 : Fuzzer Int
aFactorOf3 =
    intRange -1000 0 |> Fuzz.map ((*) 3)


aPositiveNonFactorOf3 : Fuzzer Int
aPositiveNonFactorOf3 =
    let
        makeNonFactorOf3 n =
            if (n |> modBy 3) == 0 then
                n + 1

            else
                n
    in
    Fuzz.intRange 1 1000
        |> Fuzz.map makeNonFactorOf3


aNonEmptyString : Fuzzer String
aNonEmptyString =
    let
        makeNonEmpty ( c, s ) =
            if s |> String.isEmpty then
                c |> String.fromChar

            else
                s
    in
    Fuzz.tuple ( Fuzz.char, Fuzz.string )
        |> Fuzz.map makeNonEmpty
