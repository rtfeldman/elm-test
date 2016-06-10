module Test exposing (Test, failWith, toFailures, pass, fail, fuzz, fuzz2, fuzz3, fuzz4, fuzz5)

{-| Testing

@docs Test, pass, fail, it, failWith, toFailures, fuzz, fuzz2, fuzz3, fuzz4, fuzz5
-}

import Dict
import Shrink
import Random.Pcg as Random exposing (Generator)
import Fuzzer exposing (Fuzzer)


{-| If the given test fails, replace its Fail message with the given one.

    import Test exposing (failWith)
    import Assert


    Assert.equal { expected = "foo", actual = "bar" }
        |> failWith "thought they'd be the same"
        |> Test.toFailures
        -- Just { messages = [ "thought they'd be the same" ], context = [] }
-}
failWith : String -> Test -> Test
failWith str =
    formatFailures (\_ -> str)


{-| A single test run. This can either be a [`pass`](#pass) or [`fail`](#fail).

Use [`toFailures`](#toFailures) to convert a `Test` into appropriately
contextualized failure messages.
-}
type Test
    = Pass
    | Fail { messages : List String, context : List String }


{-| A [`Test`](#Test) which failed with the given message.

-- TODO code sample
-}
fail : String -> Test
fail str =
    Fail { messages = [ str ], context = [] }


{-| A [`Test`](#Test) which passed.

-- TODO code sample
-}
pass : Test
pass =
    Pass


{-| Return contextualized failure messages from the given [`Test`](#Test).

Note that fuzz tests may return multiple failure messages from a single `Test`!

-- TODO code sample
-}
toFailures : Test -> Maybe { messages : List String, context : List String }
toFailures test =
    case test of
        Pass ->
            Nothing

        Fail record ->
            Just record


{-| Run the given tests several times, using a randomly-generated input from a
`Fuzzer` each time. By default, runs each test 100 times with different inputs,
but you can configure the run count using [`withRuns`](#withRuns).

These are called "[fuzz tests](https://en.wikipedia.org/wiki/Fuzz_testing)" because of the randomness.
You may find them elsewhere called [property-based tests](http://blog.jessitron.com/2013/04/property-based-testing-what-is-it.html),
[generative tests](http://www.pivotaltracker.com/community/tracker-blog/generative-testing), or
[QuickCheck-style tests](https://en.wikipedia.org/wiki/QuickCheck).

-- TODO code sample
-}
fuzz :
    Fuzzer a
    -> (a -> Test)
    -> Test
fuzz fuzzer fuzzSuites =
    (fuzzToThunks fuzzer) fuzzSuites


{-| Run a [fuzz test](#fuzz) using two random inputs.

This is a convenicence function that lets you skip calling `Fuzzer.tuple`.


-- TODO code sample
-}
fuzz2 :
    Fuzzer a
    -> Fuzzer b
    -> (a -> b -> Test)
    -> Test
fuzz2 fuzzA fuzzB =
    let
        fuzzer =
            Fuzzer.tuple ( fuzzA, fuzzB )
    in
        fuzzN (uncurry >> fuzzToThunks fuzzer)


{-| Run a [fuzz test](#fuzz) using three random inputs.

This is a convenicence function that lets you skip calling `Fuzzer.tuple3`.

-- TODO code sample
-}
fuzz3 :
    Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> (a -> b -> c -> Test)
    -> Test
fuzz3 fuzzA fuzzB fuzzC =
    let
        fuzzer =
            Fuzzer.tuple3 ( fuzzA, fuzzB, fuzzC )
    in
        fuzzN (uncurry3 >> fuzzToThunks fuzzer)


{-| Run a [fuzz test](#fuzz) using four random inputs.

This is a convenicence function that lets you skip calling `Fuzzer.tuple4`.

-- TODO code sample
-}
fuzz4 :
    Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> Fuzzer d
    -> (a -> b -> c -> d -> Test)
    -> Test
fuzz4 fuzzA fuzzB fuzzC fuzzD =
    let
        fuzzer =
            Fuzzer.tuple4 ( fuzzA, fuzzB, fuzzC, fuzzD )
    in
        fuzzN (uncurry4 >> fuzzToThunks fuzzer)


{-| Run a [fuzz test](#fuzz) using four random inputs.

This is a convenicence function that lets you skip calling `Fuzzer.tuple5`.

-- TODO code sample
-}
fuzz5 :
    Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> Fuzzer d
    -> Fuzzer e
    -> (a -> b -> c -> d -> e -> Test)
    -> Test
fuzz5 fuzzA fuzzB fuzzC fuzzD fuzzE =
    let
        fuzzer =
            Fuzzer.tuple5 ( fuzzA, fuzzB, fuzzC, fuzzD, fuzzE )
    in
        fuzzN (uncurry5 >> fuzzToThunks fuzzer)



-- INTERNAL HELPERS --


fuzzToThunks : Fuzzer a -> (a -> Test) -> Random.Seed -> Int -> Bool -> List Test
fuzzToThunks fuzzer runAssert seed runs doShrink =
    let
        runWithInput val =
            let
                test =
                    runAssert val

                shrunkenVal =
                    if doShrink && test /= Pass then
                        Shrink.shrink (runAssert >> (/=) Pass) fuzzer.shrinker val
                    else
                        val

                shrunkenTest =
                    if doShrink then
                        runAssert shrunkenVal
                    else
                        test
            in
                ( Just (toString shrunkenVal), shrunkenTest )

        -- testRuns : Generator (List a)
        testRuns =
            Random.list runs fuzzer.generator

        generators =
            Random.map (List.map runWithInput) testRuns

        dedupe pairs =
            pairs
                |> List.map (\( mk, v ) -> ( Maybe.withDefault "" mk, v ))
                |> Dict.fromList
                |> Dict.toList
                |> List.map
                    (\( s, v ) ->
                        ( if s == "" then
                            Nothing
                          else
                            Just s
                        , v
                        )
                    )
    in
        seed
            |> Random.step generators
            |> fst
            |> dedupe
            |> List.map formatTest


formatTest : ( Maybe String, Test ) -> Test
formatTest ( input, test ) =
    formatFailures (prependInput input) test


formatFailures : (String -> String) -> Test -> Test
formatFailures format test =
    case test of
        Fail record ->
            Fail { record | messages = List.map format record.messages }

        Pass ->
            test


prependInput : Maybe String -> String -> String
prependInput input original =
    case input of
        Nothing ->
            original

        Just str ->
            "Input: " ++ str ++ "\n\n" ++ original


fuzzN : (a -> Random.Seed -> Int -> Bool -> List Test) -> List a -> Test
fuzzN fn fuzzSuites =
    fuzzSuites
        |> List.map fn
        |> concat


uncurry3 : (a -> b -> c -> d) -> ( a, b, c ) -> d
uncurry3 fn ( a, b, c ) =
    fn a b c


uncurry4 : (a -> b -> c -> d -> e) -> ( a, b, c, d ) -> e
uncurry4 fn ( a, b, c, d ) =
    fn a b c d


uncurry5 : (a -> b -> c -> d -> e -> f) -> ( a, b, c, d, e ) -> f
uncurry5 fn ( a, b, c, d, e ) =
    fn a b c d e


concat : List Test -> Test
concat =
    concatHelp Pass



-- INTERNAL HELPERS --


concatHelp : Test -> List Test -> Test
concatHelp result tests =
    case tests of
        [] ->
            result

        Pass :: rest ->
            concatHelp result rest

        ((Fail record) as currentFailure) :: rest ->
            let
                newFailure =
                    case result of
                        Fail { messages } ->
                            -- NOTE: we use the first context we get, and
                            -- assume all other contexts are the same.
                            Fail { record | messages = record.messages ++ messages }

                        Pass ->
                            currentFailure
            in
                concatHelp newFailure rest
