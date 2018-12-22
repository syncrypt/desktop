module TestMain exposing (..)

import Char
import Expect
import Fuzz exposing (..)
import PathTests
import String
import Test exposing (..)
import Test.Runner.Html
import TooltipTests
import TutorialTests
import UtilTests


main : Test.Runner.Html.TestProgram
main =
    [ PathTests.fromString
    , PathTests.isNestedUnder
    , TutorialTests.toNextStep
    , TutorialTests.toPrevStep
    , UtilTests.bytesReadable
    , UtilTests.removeLeading
    , UtilTests.removeTrailing
    , UtilTests.skipCharsWhile
    , TooltipTests.text
    ]
        |> concat
        |> Test.Runner.Html.run
