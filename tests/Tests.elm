module Tests exposing (..)

import Test exposing (..)
import StructuredWriterTests


suite : Test
suite =
    concat
        [ StructuredWriterTests.suite
        ]
