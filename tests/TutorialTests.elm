module TutorialTests exposing (..)

import Expect
import Fuzz
import Test exposing (..)
import Translation as T
import Tutorial
import Util exposing ((~>))


type Msg
    = TutMsg Tutorial.Msg


step1 : Tutorial.Step msg
step1 =
    { id = "step1"
    , title = T.OK
    , paragraphs = [ T.OK, T.Cancel, T.Close ]
    , onEnter = []
    , onExit = []
    }


step2 : Tutorial.Step msg
step2 =
    { id = "step2"
    , title = T.Cancel
    , paragraphs = [ T.OK, T.Cancel, T.Close ]
    , onEnter = []
    , onExit = []
    }


step3 : Tutorial.Step msg
step3 =
    { id = "step3"
    , title = T.Finish
    , paragraphs = [ T.OK, T.Cancel, T.Close ]
    , onEnter = []
    , onExit = []
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
                let
                    ( step, cmd ) =
                        tut
                            |> Tutorial.toNextStep
                in
                step
                    |> Tutorial.currentStep
                    |> Expect.equal (Just step2)
        , test "goes to next step once more" <|
            \_ ->
                let
                    ( step, cmd ) =
                        tut
                            |> Tutorial.toNextStep
                            ~> Tutorial.toNextStep
                in
                step
                    |> Tutorial.currentStep
                    |> Expect.equal (Just step3)
        , test "goes to finished state when done" <|
            \_ ->
                let
                    ( tut2, _ ) =
                        tut
                            |> Tutorial.toNextStep
                            ~> Tutorial.toNextStep
                            ~> Tutorial.toNextStep

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
        ( tut, _ ) =
            Tutorial.init TutMsg step1 [ step2, step3 ]
                |> Tutorial.toNextStep
                ~> Tutorial.toNextStep
    in
    describe "Tutorial.toPrevStep"
        [ test "returns to last step if tutorial is finished / at the end" <|
            \_ ->
                let
                    ( step, _ ) =
                        tut
                            |> Tutorial.toNextStep
                            ~> Tutorial.toPrevStep
                in
                step
                    |> Tutorial.currentStep
                    |> Expect.equal (Just step3)
        , test "going back and forth works as expected and yields the same currentStep" <|
            \_ ->
                let
                    ( step, _ ) =
                        tut
                            |> Tutorial.toNextStep
                            ~> Tutorial.toPrevStep

                    ( step2, _ ) =
                        tut
                            |> Tutorial.toPrevStep
                            ~> Tutorial.toNextStep
                in
                step
                    |> Tutorial.currentStep
                    |> Expect.equal (step2 |> Tutorial.currentStep)
        , test "goes to previous step" <|
            \_ ->
                tut
                    |> Tutorial.currentStep
                    |> Expect.equal (Just step3)
        , test "goes to previous step again" <|
            \_ ->
                let
                    ( step, _ ) =
                        tut
                            |> Tutorial.toPrevStep
                in
                step
                    |> Tutorial.currentStep
                    |> Expect.equal (Just step2)
        , test "goes to previous step once more" <|
            \_ ->
                let
                    ( step, _ ) =
                        tut
                            |> Tutorial.toPrevStep
                            ~> Tutorial.toPrevStep
                in
                step
                    |> Tutorial.currentStep
                    |> Expect.equal (Just step1)
        , test "goes to first state when done" <|
            \_ ->
                let
                    ( tut2, _ ) =
                        tut
                            |> Tutorial.toPrevStep
                            ~> Tutorial.toPrevStep
                            ~> Tutorial.toPrevStep

                    currStep =
                        tut2 |> Tutorial.currentStep

                    isFinished =
                        tut2 |> Tutorial.isFinished
                in
                ( currStep, isFinished )
                    |> Expect.equal ( Just step1, False )
        ]
