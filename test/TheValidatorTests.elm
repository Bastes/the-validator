module TheValidatorTests exposing (theValidatorTests)

import TheValidator as Validator exposing (..)
import Expect
import Test exposing (..)
import Fuzz exposing (..)


theValidatorTests : Test
theValidatorTests =
    let
        aValidator =
            Validator.simple ((/=) "") "Hey, this string is blank! WTF dude?"
    in
        describe "TheValidator"
            [ describe "validate"
                [ test "show the error when there are errors" <|
                    \_ ->
                        validate aValidator ""
                            |> Expect.equal [ "Hey, this string is blank! WTF dude?" ]
                , test "show no error when there are no errors" <|
                    \_ ->
                        validate aValidator "non-blank"
                            |> Expect.equal []
                ]
            , describe "isValid"
                [ test "it is True when the model is valid" <|
                    \_ ->
                        isValid aValidator "absolutely not blank"
                            |> Expect.equal True
                , test "it is False when the model is invalid" <|
                    \_ ->
                        isValid aValidator ""
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
        , fallback = (always 7)
        , condition = (\n -> (n % 3 /= 0) && (n % 5 /= 0))
        }
        (intRange 1 1000)
