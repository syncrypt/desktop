module TutorialTests exposing (..)

import Expect
import Fuzz
import Test exposing (..)
import Tutorial


type Msg
    = TutMsg Tutorial.Msg


step1 : Tutorial.Step
step1 =
    { id = "step1"
    , title = "Step 1"
    , paragraphs = [ "hello", "world" ]
    }


step2 : Tutorial.Step
step2 =
    { id = "step2"
    , title = "Step 2"
    , paragraphs = [ "hello", "world", "again" ]
    }


step3 : Tutorial.Step
step3 =
    { id = "step3"
    , title = "Step 3"
    , paragraphs = [ "hello", "world", "once", "more" ]
    }


toNextStep : Test
toNextStep =
    let
        tut =
            Tutorial.init TutMsg step1 [ step2, step3 ]
    in
    describe "Tutorial.toNextStep"
        [ test "goes to next step" <|
            \_ ->
                tut
                    |> Tutorial.currentStep
                    |> Expect.equal (Just step1)
        , test "goes to next step again" <|
            \_ ->
                tut
                    |> Tutorial.toNextStep
                    |> Tutorial.currentStep
                    |> Expect.equal (Just step2)
        , test "goes to next step once more" <|
            \_ ->
                tut
                    |> Tutorial.toNextStep
                    |> Tutorial.toNextStep
                    |> Tutorial.currentStep
                    |> Expect.equal (Just step3)
        , test "goes to finished state when done" <|
            \_ ->
                let
                    tut2 =
                        tut
                            |> Tutorial.toNextStep
                            |> Tutorial.toNextStep
                            |> Tutorial.toNextStep

                    currStep =
                        tut2 |> Tutorial.currentStep

                    isFinished =
                        tut2 |> Tutorial.isFinished
                in
                ( currStep, isFinished )
                    |> Expect.equal ( Nothing, True )
        ]


toPrevStep : Test
toPrevStep =
    let
        tut =
            Tutorial.init TutMsg step1 [ step2, step3 ]
                |> Tutorial.toNextStep
                |> Tutorial.toNextStep
    in
    describe "Tutorial.toPrevStep"
        [ test "returns to last step if tutorial is finished / at the end" <|
            \_ ->
                tut
                    |> Tutorial.toNextStep
                    |> Tutorial.toPrevStep
                    |> Tutorial.currentStep
                    |> Expect.equal (Just step3)
        , test "going back and forth works as expected and yields the same currentStep" <|
            \_ ->
                tut
                    |> Tutorial.toNextStep
                    |> Tutorial.toPrevStep
                    |> Tutorial.currentStep
                    |> Expect.equal
                        (tut
                            |> Tutorial.toPrevStep
                            |> Tutorial.toNextStep
                            |> Tutorial.currentStep
                        )
        , test "goes to previous step" <|
            \_ ->
                tut
                    |> Tutorial.currentStep
                    |> Expect.equal (Just step3)
        , test "goes to previous step again" <|
            \_ ->
                tut
                    |> Tutorial.toPrevStep
                    |> Tutorial.currentStep
                    |> Expect.equal (Just step2)
        , test "goes to previous step once more" <|
            \_ ->
                tut
                    |> Tutorial.toPrevStep
                    |> Tutorial.toPrevStep
                    |> Tutorial.currentStep
                    |> Expect.equal (Just step1)
        , test "goes to first state when done" <|
            \_ ->
                let
                    tut2 =
                        tut
                            |> Tutorial.toPrevStep
                            |> Tutorial.toPrevStep
                            |> Tutorial.toPrevStep

                    currStep =
                        tut2 |> Tutorial.currentStep

                    isFinished =
                        tut2 |> Tutorial.isFinished
                in
                ( currStep, isFinished )
                    |> Expect.equal ( Just step1, False )
        ]
