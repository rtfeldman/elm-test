module Suite exposing (Suite, toRunners, batch, describe, withRuns, withSeed, withShrink)

{-| A collection of Tests.

@docs Suite, toRunners, batch, describe, withRuns, withSeed, withShrink
-}

import Test exposing (Test)


{-| A batch of Tests which have yet to be evaluated. Execution order is not
guaranteed.

Use [`toRunners`](#toRunners) to convert a `Suite` into a list of
`() -> Test` functions, which can then be evaluated.
-}
type Suite
    = Tests Options (List (Random.Seed -> Int -> Bool -> List Test))
    | Batch (List Suite)


{-| Turn a `Suite` into a list of thunks that can be run to produce Tests.

-- TODO code example
-}
toRunners : Random.Seed -> Suite -> List (() -> Test)
toRunners seed suite =
    case suite of
        Tests opts thunks ->
            List.concatMap (toTests seed opts) thunks

        Batch suites ->
            List.concatMap (toRunners seed) suites


{-| Override the default run count for fuzz tests in this suite. (By default,
[fuzz tests](#fuzz) run 100 times with randomly-generated inputs on each run.)

-- TODO give an example with nested calls to illustrate how nested overrides work. --
-}
withRuns : Int -> Suite -> Suite
withRuns runs =
    mapOptions (\opts -> { opts | runs = Maybe.oneOf [ opts.runs, Just runs ] })


{-| Override the default initial random number seed for fuzz tests in this suite.
(Most runners initialize [fuzz tests](#fuzz) with a seed based on the current
time when the runner starts up, but some use other approaches.)

-- TODO give an example with nested calls to illustrate how nested overrides work. --
-}
withSeed : Random.Seed -> Suite -> Suite
withSeed seed =
    mapOptions (\opts -> { opts | seed = Maybe.oneOf [ opts.seed, Just seed ] })


{-| Override the default shrinking setting for fuzz tests in this suite.
(By default, [fuzz tests](#fuzz) have this set to `True`, meaning they will
perform shrinking when they fail. Shrinking searches for the minimal set of
inputs necessary to reproduce a failure, so you get a more concise error report
when a fuzz test fails. It's nice, but not instantaneous. This setting does not
matter when all tests pass, since shrinking happens only to failures.)

-- TODO give an example with nested calls to illustrate how nested overrides work. --
-}
withShrink : Bool -> Suite -> Suite
withShrink doShrink =
    mapOptions (\opts -> { opts | doShrink = Maybe.oneOf [ opts.doShrink, Just doShrink ] })


{-| Run a list of Suites.

See [`describe`](#describe) for running Suites with a descriptive string.

-- TODO give a code example.
-}
batch : List Suite -> Suite
batch =
    Batch


{-| Apply a description to each `Suite` in the given list, then [`batch`](#batch)
them.

-- TODO give a code example.
-}
describe : String -> List Suite -> Suite
describe desc =
    List.map (mapOptions (prependFail desc)) >> batch


test : (a -> Test) -> a -> Suite
test thunk arg =
    Tests initialUnitOptions (\_ -> [ thunk arg ])


it : String -> (a -> Test) -> a -> Suite
it desc thunk arg =
    Tests initialUnitOptions (\_ -> [ thunk arg ])
        |> mapOptions (prependFail desc)


initialUnitOptions : Options
initialUnitOptions =
    { onFail = []
    , runs = Just 1
    , doShrink = Just False
    , seed = Just defaults.seed
    }


prependFail : String -> Options -> Options
prependFail str opts =
    { opts | onFail = str :: opts.onFail }


toTests : Random.Seed -> Options -> (Options -> List Test) -> List (() -> Test)
toTests seed opts getTests =
    { opts
        | seed = Maybe.oneOf [ opts.seed, Just seed ]
        , runs = Maybe.withDefault defaults.runs opts.runs
        , doShrink = Maybe.withDefault defaults.doShrink opts.doShrink
    }
        |> getTests
        |> List.map (\test _ -> List.foldr Test.it test opts.onFail)


mapOptions : (Options -> Options) -> Suite -> Suite
mapOptions translate suite =
    case suite of
        Tests opts thunks ->
            Tests (translate opts) thunks

        Batch suites ->
            suites
                |> List.map (mapOptions translate)
                |> Batch


type alias Options =
    { onFail : List String
    , runs : Maybe Int
    , doShrink : Maybe Bool
    , seed : Maybe Random.Seed
    }


initialOptions : Options
initialOptions =
    { onFail = []
    , runs = Nothing
    , doShrink = Nothing
    , seed = Nothing
    }


defaults : { runs : Int, doShrink : Bool, seed : Random.Seed }
defaults =
    { runs = 100
    , doShrink = True
    , seed = Random.initialSeed 42
    }
