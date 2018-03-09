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
            [ test "reports the error when there is one" <|
                \_ ->
                    validate aValidator ""
                        |> Expect.equal [ "Hey, this string is blank! WTF dude?" ]
            , test "show no error when there is none" <|
                \_ ->
                    validate aValidator "non-blank"
                        |> Expect.equal []
            , test "validates a valid model" <|
                \_ ->
                    isValid aValidator "absolutely not blank"
                        |> Expect.equal True
            , test "rejects an invalid model" <|
                \_ ->
                    isValid aValidator ""
                        |> Expect.equal False
            ]
        , describe "parameterized" <|
            let
                aValidator =
                    Validator.parameterized
                        (flip (<) 3)
                        (\n -> toString n ++ " should be lower than 3!")
            in
            [ fuzz (intRange 3 1000) "reports the error when there is one, using the parameter" <|
                \n ->
                    validate aValidator n
                        |> Expect.equal [ toString n ++ " should be lower than 3!" ]
            , fuzz (intRange -1000 2) "reports no error when there is none" <|
                \n ->
                    validate aValidator n
                        |> Expect.equal []
            , fuzz (intRange -1000 2) "validates a valid model" <|
                \n ->
                    isValid aValidator n
                        |> Expect.equal True
            , fuzz (intRange 3 1000) "rejects an invalid model" <|
                \n ->
                    isValid aValidator n
                        |> Expect.equal False
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
            [ test "it reports all errors when all are triggered" <|
                \_ ->
                    validate noneOfTheseFizzAndBuzzes 15
                        |> Expect.equal [ "not fizz!", "not buzz!", "not fizzbuzz!" ]
            , test "it reports only the subset of errors that are triggered" <|
                \_ ->
                    validate noneOfTheseFizzAndBuzzes 5
                        |> Expect.equal [ "not buzz!" ]
            , fuzz aNonFizzBuzzInt "it reports nothing when there's nothing to report" <|
                \n ->
                    validate noneOfTheseFizzAndBuzzes n
                        |> Expect.equal []
            , fuzz aNonFizzBuzzInt "it is valid when all validators pass" <|
                \n ->
                    isValid noneOfTheseFizzAndBuzzes n
                        |> Expect.equal True
            , fuzz aFizzBuzzInt "it is invalid as soon as a validator refuses" <|
                \n ->
                    isValid noneOfTheseFizzAndBuzzes n
                        |> Expect.equal False
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
                        |> Validator.map (\error -> [ "the number", error ])
            in
            [ fuzz aFactorOf3 "it maps over all errors" <|
                \n ->
                    validate positiveAndNotFactorOf3 n
                        |> Expect.equal
                            [ [ "the number", "needs to be positive" ]
                            , [ "the number", toString n ++ " needs not be a factor of 3" ]
                            ]
            , fuzz aNonFactorOf3 "it does not return errors when there are none" <|
                \n ->
                    validate positiveAndNotFactorOf3 n
                        |> Expect.equal []
            , fuzz aFactorOf3 "it rejects an invalid model" <|
                \n ->
                    isValid positiveAndNotFactorOf3 n
                        |> Expect.equal False
            , fuzz aNonFactorOf3 "it validates a valid model" <|
                \n ->
                    isValid positiveAndNotFactorOf3 n
                        |> Expect.equal True
            ]
        ]



-- HELPERS


oneOfThese : List a -> Fuzzer a
oneOfThese =
    oneOf << List.map constant


aFizzBuzzInt : Fuzzer Int
aFizzBuzzInt =
    constant (*)
        |> andMap (oneOfThese [ 3, 5, 15 ])
        |> andMap (intRange 1 1000)


aNonFizzBuzzInt : Fuzzer Int
aNonFizzBuzzInt =
    conditional
        { retries = 10
        , fallback = always 7
        , condition = \n -> (n % 3 /= 0) && (n % 5 /= 0)
        }
        (intRange 1 1000)


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
