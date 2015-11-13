module ElmTest.Run where

{-| Basic utilities for running tests and customizing the output. If you don't care about customizing
the output, instead look at the ```runDisplay``` series in ElmTest.Runner

# Run
@docs run, pass, fail, Result, failedSuites, failedTests, passedSuites, passedTests

-}

import ElmTest.Assertion exposing (..)
import ElmTest.Test exposing (..)
import List


{-| The result of a test or suite of tests. Could be `Pass`, `Fail`, or a more
detailed `Report` of several passes, failrures, and `Result`s. -}
type Result = Pass String
            | Fail String String
            | Report String { results  : List Result
                            , passes   : List Result
                            , failures : List Result
                            }

{-| Run a test and get a Result -}
run : Test -> Result
run test =
    case test of
        TestCase name assertion -> let runAssertion t m = if t ()
                                                          then Pass name
                                                          else Fail name m
                                in case assertion of
                                     AssertEqual t a b    -> runAssertion t <| "Expected: " ++ a ++ "; got: " ++ b
                                     AssertNotEqual t a b -> runAssertion t <| a ++ " equals " ++ b
                                     AssertTrue  t        -> runAssertion t <| "not True"
                                     AssertFalse t        -> runAssertion t <| "not False"
        Suite name tests -> let results = List.map run tests
                                (passes, fails) = List.partition pass results
                            in Report name { results  = results
                                           , passes   = passes
                                           , failures = fails
                                           }

{-| Transform a Result into a Bool. True if the result represents a pass, otherwise False -}
pass : Result -> Bool
pass m = case m of
           Pass _     -> True
           Fail _ _   -> False
           Report _ r -> if (List.length (.failures r) > 0) then False else True

{-| Transform a Result into a Bool. True if the result represents a fail, otherwise False -}
fail : Result -> Bool
fail = not << pass


{-| The number of tests that passed in the Result. -}
passedTests : Result -> Int
passedTests result = case result of
                        Pass _     -> 1
                        Fail _ _   -> 0
                        Report n r -> List.sum << List.map passedTests <| r.results


{-| The number of tests that failed in the Result. -}
failedTests : Result -> Int
failedTests result = case result of
                        Pass _     -> 0
                        Fail _ _   -> 1
                        Report n r -> List.sum << List.map failedTests <| r.results


{-| The number of test suites that passed in the Result. -}
passedSuites : Result -> Int
passedSuites result = case result of
                        Report n r -> let passed = if List.length r.failures == 0
                                                   then 1
                                                   else 0
                                      in  passed + (List.sum << List.map passedSuites <| r.results)
                        _ -> 0


{-| The number of test suites that failed in the Result. -}
failedSuites : Result -> Int
failedSuites result = case result of
                        Report n r -> let failed = if List.length r.failures > 0
                                                   then 1
                                                   else 0
                                      in  failed + (List.sum << List.map failedSuites <| r.results)
                        _ -> 0
