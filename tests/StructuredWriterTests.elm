module StructuredWriterTests exposing (suite)

import StructuredWriter as Writer exposing (..)
import Test exposing (Test, test, describe)
import Expect


suite : Test
suite =
    describe "StructuredWriter"
        [ test "write single line parensComma" <|
            \() ->
                Writer.write
                    (parensComma False
                        [ (string "x")
                        , (string "y")
                        ]
                    )
                    |> Expect.equal "(x, y)"
        , test "write multi line parensComma" <|
            \() ->
                Writer.write
                    (parensComma True
                        [ (string "x")
                        , (string "y")
                        ]
                    )
                    |> Expect.equal "(x\n, y)"
        , test "indented breaked" <|
            \() ->
                Writer.write (indent 2 (breaked [ string "a", string "b" ]))
                    |> Expect.equal "  a\n  b"
        , test "epsilon" <|
            \() ->
                Writer.write Writer.epsilon
                    |> Expect.equal ""
        , test "indented epsilon" <|
            \() ->
                Writer.write (Writer.indent 2 Writer.epsilon)
                    |> Expect.equal "  "
        , test "spaced" <|
            \() ->
                Writer.write (Writer.spaced [ Writer.string "a", Writer.string "b", Writer.string "c" ])
                    |> Expect.equal "a b c"
        , test "maybe nothing" <|
            \() ->
                Writer.write (Writer.maybe Nothing)
                    |> Expect.equal ""
        , test "maybe just" <|
            \() ->
                Writer.write (Writer.maybe (Just (Writer.string "a")))
                    |> Expect.equal "a"
        , test "join" <|
            \() ->
                Writer.write (Writer.join [ Writer.string "foo", Writer.string "bar" ])
                    |> Expect.equal "foobar"
        , test "indented sep with breaking lines" <|
            \() ->
                Writer.indent 2
                    (Writer.breaked
                        [ Writer.string "foo"
                        , Writer.indent 2
                            (Writer.spaced
                                [ Writer.string "bar"
                                , Writer.sepBySpace True [ Writer.string "baz", Writer.string "qux" ]
                                ]
                            )
                        ]
                    )
                    |> Writer.write
                    |> Expect.equal
                        (""
                            ++ "  foo\n"
                            ++ "    bar baz\n"
                            ++ "     qux"
                        )
        ]
