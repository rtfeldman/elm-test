module ElmTest.Runner.Console (runDisplay) where

{-| Run a test suite as a command-line script.

# Run
@docs runDisplay

-}

import List
import String

import Console exposing (..)

import ElmTest.Test exposing (..)
import ElmTest.Run as Run
import ElmTest.Runner.String as String

{-| Run a list of tests in the IO type from [Laszlo Pandy's Elm Console library](https://github.com/laszlopandy/elm-console/).
Requires this library to work. Results are printed to console once all tests have completed. Exits with
exit code 0 if all tests pass, or with code 1 if any tests fail.
-}
runDisplay : Test -> IO ()
runDisplay tests =
    let ((summary, allPassed) :: results) = String.run tests
        out = summary ++ "\n\n" ++ (String.concat << List.intersperse "\n" << List.map fst <| results)
    in putStrLn out >>>
       case Run.pass allPassed of
            True  -> exit 0
            False -> exit 1
