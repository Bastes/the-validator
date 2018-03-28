# Elm's The Validator

A library to help you validate your models, point errors and prevent accidental
recording of bad data.

## Why use The Validator?

Sometimes, HTML5 errors are not enough. Sometimes, you need to validate a string
with Elm code, like the super-smart human being you are that don't believe in
regular expressions. Sometimes, you'd like to not repeat yourself and easily
name, compose and reuse your validations to provide useful feedback or just to
block that damned "Save" button until the user gives you proper input.

Sometimes, you need TheValidator.

```elm

import TheValidator

type alias Error =
  String

type alias Email =
  String

validEmail : Validator Email Error
validEmail =
  TheValidator.simple isValidEmail "is not a valid email"

view : Model -> Html Msg
view model =
  let
    whenInvalid =
      not (TheValidator.isValid validEmail model)

    emailErrors =
      model
        |> TheValidator.validate validEmail
        |> String.join "\n"

  in
  div []
    [ input
      -- add the "error" class when the the model is invalid
      [ classList [("error", whenInvalid)]
      -- display all errors on the email field (if any) in the title
      , title emailErrors
      , value model
      , onChange UpdateEmail
      ]
      []
    , input
      [ _type "submit"
      , onClick SubmitForm
      -- disable the submit button if the model is invalid
      , disabled whenInvalid
      ]
    ]
```

This basic example showcases how you can write a simple validator for an email
(assuming you've got an `isValidEmail` validation fuction), use it in your
view code to add error classes when needed, display an explaination for it
(in the title of the input) and disable the form submission when your validation
don't pass.

### Now, most forms have more than one field right?

This is where TheValidator starts to shine. Consider this model:

```elm
type alias Model =
  { firstName : String
  , lastName : String
  , email: String
  , age : Int
  }
```

Already, we've may have problems here:
* the age is an Int ; we can use a numeric input, but if for some reason we
don't accept models whose age is a factor of 7, that'll be a pain to validate
and prode feedback with html5 only
* the firstName and lastName may share some validations (ie: nonBlank) and at
the same time not share others (ie: no-one named "Montague" since we recruit for
CapuletCo.) and we don't want to have to re-write validations that serve the
same purpose
* we want to show only the reasons of the errors for each specific field on
that specific field while at the same time offer a recap of all the errors
upfront with references to the incriminated fields when we hover over the submit
button

Well, let's:

```elm
import TheValidator exposing (isValid, validate)

nonBlank : Validation Error String
nonBlank =
  TheValidator.simple isNotBlank "should not be blank"

nonMontague : Validation Error String
nonMontague =
  TheValidator.simple (\name -> name /= "Montague") "should not be \"Montague\""

nonFactorOf7 : Validation Error Int
nonFactorOf7 =
  TheValidator.simple (\age -> (age % 7) /= 0) "should not be a factor of 7"

nonNegative : Validation Error Int
nonNegative =
  TheValidator.simple (\age -> age >= 0) "should not be negative"

-- now we focus the nonBlank validation on the firstName field
validFirstName = Validator Model Error
validFirstName =
  nonBlank
    |> TheValidator.focusError
      -- that's how this new validator knows what to validate in the whole model
      .firstName
      -- that's what this new validator adds to the errors to point to the field
      (\_ error -> "the first name " ++ error)

-- same thing with the lastName with a twist: we combine two validations first
validLastName = Validator Model Error
validLastName =
  -- this method combine all validators in the list into one
  TheValidator.all
    [ nonBlank
    , nonMontague
    ]
    |> TheValidator.focusError
      .lastName
      (\model error -> "the last name " ++ error)

validAge = Validator Model Error
validAge =
  TheValidator.all
    [ nonNegative
    , nonFactorOf7
    ]
    |> TheValidator.focus
      .age
      (\model error -> "the age" ++ error)

-- now we bring everything together to validate the whole model
validModel =
  TheValidator.all
    [ validFirstName
    , validLastName
    , validEmail
    , validAge
    ]

view : Model -> Html Msg
view ({ firstName, lastName, email, age } as model) =
  div []
    [ input
      [ classList [("error", isValid validFirstName firstName )]
      , title (validate validFirstName firstName |> String.join "\n")
      , value firstName
      , ...
      ]
      []
    , input
      [ classList [("error", isValid validLastName lastName )]
      , title (validate validLastName lastName |> String.join "\n")
      , value lastName
      , ...
      ]
      []
    , input
      [ classList [("error", isValid validEmail email )]
      , title (validate validEmail email |> String.join "\n")
      , value email
      , ...
      ]
      []
    , input
      [ classList [("error", isValid validAge age )]
      , title (validate validAge age |> String.join "\n")
      , value (age |> toString)
      , ...
      ]
      []
    , input
      [ _type "submit"
      , onClick SubmitForm
      , disabled (model |> isValid validModel)
      , title (model |> validate validModel)
      ]
    ]
```

Now this is nice, all our validation logic is held by the validators and we can
re-use common validation logic between fields of the same type that share
identical properties.

### But what if we want to validate a list of element?

When we validate a list of elements, we want to point to each element inside
the list its own failures (by its properties, by its index, etc.). How would we
do that with TheValidator?

Easily:

```elm
type alias Models =
  List Model

validModels =
  validModel
    |> TheValidator.list
      (\index model error ->
        "#" ++ index ++ " (" ++ model.firstName ++ " " ++ model.lastName ++ ") " ++ error
      )
```

The `validModels` validator will validate a `List Model` and pass only when all
items pass, returning all error with the index, first and last name of the
incriminated item.


## Credits

This library was freely inspired by rtfeldman's (https://github.com/rtfeldman),
elm-validate (https://github.com/rtfeldman/elm-validate) library.
I would have used it instead but for the lack of ability to nest validations
deeply.

## Boring license stuff

This library is licenced under GPLv3. Check out the LICENSE file for more on
this topic.

## I'll be back

```
¶¶¶8¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶188¶¶811_18¶¶¶8
¶¶¶6_2¶¶¶¶¶¶88666¶¶¶¶¶¶¶¶8¶¶¶88¶¶¶¶¶¶¶¶¶6________¶
¶¶¶6_2¶11___8668¶¶¶¶¶¶¶¶¶_¶¶8¶¶¶¶¶¶¶¶¶¶¶¶8¶62____2
¶¶6______288¶¶¶¶¶¶¶¶61¶¶6_22_6¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶1_16¶
¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶2__________________6816¶¶¶1111_11
¶¶8¶¶¶¶¶¶¶¶¶¶168________________________1¶66¶¶¶228
¶¶¶¶¶¶¶¶¶¶¶__18___________________________2¶¶¶¶¶¶¶
¶¶¶¶¶¶¶¶¶¶___2______________________________¶¶8¶¶¶
¶¶¶¶¶¶¶¶¶¶1_2________________________________¶¶88¶
¶¶¶¶¶¶¶¶¶¶¶¶82_______________________________¶¶¶¶¶
¶¶¶¶¶¶¶¶¶¶¶¶¶¶2______________________________¶¶¶¶¶
¶¶¶¶¶¶¶¶¶¶¶¶¶¶61_____________________________¶¶¶¶¶
¶¶¶¶¶¶¶¶¶¶¶¶¶_________________________________1¶¶¶
¶¶¶¶¶¶¶¶¶¶¶¶¶28¶¶¶¶¶¶8_8¶¶¶¶222621¶¶¶¶8¶¶¶¶66¶¶¶¶¶
¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶6¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶
¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶6¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶
¶¶¶¶¶¶¶¶1¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶__¶¶¶¶¶¶¶¶¶868¶¶¶¶¶¶
¶¶¶¶¶¶¶¶8¶¶¶¶¶¶¶¶¶¶¶¶¶¶_¶¶¶¶¶____¶¶¶_¶¶¶¶2_1¶¶¶¶¶¶
¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶1_____¶¶6¶¶¶¶¶¶¶¶¶¶¶¶¶
¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶______1¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶
¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶8________2¶¶¶¶¶¶¶¶¶¶¶¶¶¶
¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶28___________26¶¶¶¶¶¶¶¶¶¶¶
¶¶¶¶¶¶¶¶¶¶¶2211__________8__¶¶6___2____________¶¶8
¶¶_¶¶¶¶¶¶¶¶82622_1_____2_1¶6¶¶¶¶¶¶¶¶6661_11622¶¶¶8
¶1_¶¶¶¶¶¶¶¶¶¶8866266666___¶¶¶¶2_12¶¶2_188688¶8¶¶¶8
¶¶61¶¶¶¶¶¶¶¶¶¶888¶¶¶¶1_____2268¶¶¶______8¶¶¶8¶¶¶¶¶
¶¶¶_16¶¶¶¶¶¶¶¶¶¶¶¶¶¶1____________________¶¶¶¶¶¶¶¶¶
¶¶¶8621¶¶¶¶¶¶¶¶¶¶¶¶2___2128¶¶¶¶¶¶¶¶¶8_____¶¶¶¶8¶¶¶
¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶1___6¶¶¶¶2________¶¶¶2___¶¶8¶¶¶¶
¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶8_____821____________66___¶¶8¶¶¶¶
¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶1______226688¶8886______8¶8¶¶¶¶
¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶21_112222622226886___28¶¶¶¶¶¶¶
¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶8826221______6¶¶8¶¶¶¶¶¶¶¶¶¶
¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶6_______________6¶¶¶¶6¶¶¶¶¶
¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶8________6262____¶¶¶¶_¶¶¶¶¶
¶¶¶¶¶¶¶¶¶¶¶8¶¶¶¶¶¶¶¶¶¶¶¶¶¶66666866688¶¶¶¶¶¶¶_¶¶¶¶8
¶¶¶¶¶¶¶¶¶¶¶¶¶66¶¶¶¶¶¶¶¶¶¶¶¶888868668¶¶¶¶¶8¶8_¶¶¶¶¶
¶¶¶¶¶¶¶¶¶¶¶¶¶8628¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶88¶¶8_¶¶¶¶¶
____¶¶¶¶¶¶¶¶¶88666¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶¶868¶¶6_8¶¶¶¶
¶¶___1¶¶¶¶¶¶¶¶8¶868¶¶¶622_________2¶¶¶688¶8612¶¶¶¶
____8___¶¶¶¶¶¶¶8¶88¶¶¶¶66________18¶¶¶88¶¶826_¶¶¶¶
_____¶___¶¶¶¶¶886866¶¶¶¶6611___116¶¶¶¶8¶¶868616¶¶¶
_____¶1_¶¶¶¶¶¶¶¶¶8826¶¶¶8662212168¶¶¶¶¶8866262_2¶¶
____2¶_¶¶¶¶¶¶¶¶¶8¶62_2¶¶¶8666622288¶¶¶88222666_¶_1
____¶¶_¶¶¶¶¶¶¶¶¶¶8811_688222222126¶888826262661¶¶_
___¶¶86¶¶______¶¶¶¶822266212121216888888662226_¶¶¶
```

In the meantime, don't hesitate to post issues and/or ideas for improvement :)
