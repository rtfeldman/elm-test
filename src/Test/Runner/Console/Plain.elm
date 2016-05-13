module Test.Runner.Console.Plain exposing (summaryLines, logSummaryLines)

{-| Run a test suite as a command-line script.

# Run
@docs plain

-}

import List
import Test exposing (Test, ResultTree(Branch, Leaf))
import String


{-| Run a test and print the output
-}
summaryLines : Int -> ResultTree -> ( Int, List String ) -> ( Int, List String )
summaryLines indentation resultTree ( failures, lines ) =
    case resultTree of
        Leaf failureMsg pairs ->
            if List.isEmpty pairs then
                ( failures, lines )
            else
                let
                    msg : String
                    msg =
                        "✗ " ++ Maybe.withDefault "Test Failed" failureMsg

                    formatKeyValue : ( String, String ) -> String
                    formatKeyValue ( key, value ) =
                        "  • " ++ key ++ ": " ++ value

                    newLines : List String
                    newLines =
                        (msg :: List.map formatKeyValue pairs)
                            |> List.concatMap (\str -> [ str, "" ])
                            |> List.map (indent indentation)
                in
                    ( failures + 1
                    , lines ++ newLines
                    )

        Branch failureMsg children ->
            let
                ( newLines, nextIndentation ) =
                    case failureMsg of
                        Just msg ->
                            ( [ "✗ " ++ msg, "|" ]
                                |> List.map (indent indentation)
                            , indentation + 1
                            )

                        Nothing ->
                            ( [], indentation )
            in
                List.foldl (summaryLines nextIndentation) ( failures, lines ++ newLines ) children


indent : Int -> String -> String
indent amount str =
    (String.repeat amount ("| ")) ++ str


logSummaryLines : ( Int, List String ) -> ()
logSummaryLines ( failures, lines ) =
    let
        output =
            "\n" ++ (String.join "\n" lines) ++ "\n\n"
    in
        if failures > 0 then
            output
                ++ (toString failures ++ " TESTS FAILED!\n\nExit code")
                |> (flip Debug.log 1)
                |> (\_ -> Debug.crash "FAILED TEST RUN")
                |> (\_ -> ())
        else
            output
                ++ "ALL TESTS PASSED!\n\nExit code"
                |> (flip Debug.log 0)
                |> (\_ -> ())
