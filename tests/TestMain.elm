module TestMain exposing (..)

import PathTests
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
    , TooltipTests.activateAndDeactivate
    , TooltipTests.hasTooltip
    , TooltipTests.length
    ]
        |> concat
        |> Test.Runner.Html.run
