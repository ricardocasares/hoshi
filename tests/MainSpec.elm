module MainSpec exposing (suite)

import Expect exposing (equal)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Main"
        [ Test.test "numbers works" <|
            \_ -> equal 2 2
        ]
