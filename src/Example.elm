module Example exposing (..)

import Test exposing (..)
import Random.Pcg as Random
import Test.Runner.Console as Runner


{-| A fuzzzer that usually generates "foo", but occasonally "bar". We expect a claim that it's always "foo" to fail.
-}
usuallyFoo : Fuzzer String
usuallyFoo =
    Fuzzer
        (Random.oneIn 30
            |> Random.map
                (\b ->
                    if b then
                        "bar"
                    else
                        "foo"
                )
        )


actualFuzzSuite : Test
actualFuzzSuite =
    Test.fuzz usuallyFoo
        [ \shouldBeFoo ->
            { expected = "foo"
            , actually = shouldBeFoo
            }
                |> assertEqual
                |> describe "It wasn't \"foo\"."
        ]



--main : Html a
--main =
--    Html.text (toString <| runWithSeed (Random.initialSeed 42) actualFuzzSuite)


main : Program Never
main =
    Runner.plain allSuites



{- After this point, we're really just showing that Richard's proposed API compiles. -}


{-| stubbed function under test
-}
oxfordify : a -> b -> c -> String
oxfordify _ _ _ =
    "Alice, Bob, and Claire"


{-| Stubbed fuzzer
-}
string : Fuzzer String
string =
    Fuzzer
        <| Random.map
            (\b ->
                if b then
                    "foo"
                else
                    "bar"
            )
            Random.bool


allSuites : Test
allSuites =
    Test.batch [ sampleSuite ]



--Test.batch [ fuzzSuite, actualFuzzSuite, oxfordifySuite, sampleSuite ]


fuzzSuite : Test
fuzzSuite =
    (Test.fuzz2 string string)
        [ \name punctuation ->
            { expected = ""
            , actually = oxfordify "This sentence is empty" "." []
            }
                |> assertEqual
                |> describe "given an empty list, return an empty string"
        , \name punctuation ->
            { expected = "This sentence contains one item."
            , actually = oxfordify "This sentence contains " "." [ "one item" ]
            }
                |> assertEqual
        , \name punctuation ->
            { expected = "This sentence contains one item and two item."
            , actually = oxfordify "This sentence contains " "." [ "one item", "two item" ]
            }
                |> assertEqual
                |> describe "given an empty list, return an empty string"
        , \name punctuation ->
            { expected = "This sentence contains one item, two item, and three item."
            , actually = oxfordify "This sentence contains " "." [ "one item", "two item", "three item" ]
            }
                |> assertEqual
                |> describe "given a list of length 3, return an oxford-style sentence"
        ]
        |> describe "oxfordify failed"


oxfordifySuite : Test
oxfordifySuite =
    Test.unit
        [ \_ ->
            Test.unit
                [ \_ ->
                    { expected = ""
                    , actually = oxfordify "This sentence is empty" "." []
                    }
                        |> assertEqual
                        |> describe "returns an empty string"
                ]
                |> describe "given an empty list"
        , \_ ->
            { expected = "This sentence contains one item."
            , actually = oxfordify "This sentence contains " "." [ "one item" ]
            }
                |> assertEqual
        , \_ ->
            { expected = "This sentence contains one item and two item."
            , actually = oxfordify "This sentence contains " "." [ "one item", "two item" ]
            }
                |> assertEqual
                |> describe "given an empty list, did not return an empty string"
        , \_ ->
            { expected = "This sentence contains one item, two item, and three item."
            , actually = oxfordify "This sentence contains " "." [ "one item", "two item", "three item" ]
            }
                |> assertEqual
                |> describe "given a list of length 3, did not return an oxford-style sentence"
        ]
        |> describe "oxfordify failed"


sampleSuite : Test
sampleSuite =
    Test.unit
        [ \_ ->
            { expected = "a thing"
            , actually = "a thing"
            }
                |> assertEqual
                |> describe "okay, this was NOT supposed to fail"
        , \_ ->
            { expected = "failure w/o custom message"
            , actually = "kaboom!"
            }
                |> assertEqual
        , \_ ->
            { expected = "failure w/ custom message"
            , actually = "kaboom!"
            }
                |> assertEqual
                |> describe "this failed, as expected!"
        ]
        |> describe "sampleSuite failed"
