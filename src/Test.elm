module Test exposing (..)

import Random.Pcg as Random


-- none of the types below will be exported, except Test which will be opaque


type alias Outcome =
    List ( String, String )


{-| A TestTree is either
   * A leaf thunk that yields an outcome
   * A node whose children are lazy thunks
   * A node whose children are lazy and randomized thunks
   * A node whose children are already evaluated
-}
type TestTree
    = Thunk (() -> Outcome)
    | Group (List (() -> Test))
    | FuzzGroup (List (( Random.Seed, Int, Bool ) -> Test))
      -- seed, runs, doShrink; make record?
    | Batch (List Test)


type alias Options =
    { onFail : Maybe String
    , runs : Int
    , doShrink : Bool
    }


type Test
    = Test Options TestTree


type Fuzzer a
    = -- TODO: shrinking
      Fuzzer (Random.Generator a)


batch : List Test -> Test
batch tests =
    Test { onFail = Nothing, runs = 1, doShrink = False } (Batch tests)


onFail : String -> Test -> Test
onFail str (Test opts tree) =
    Test { opts | onFail = Just str } tree


runs : Int -> Test -> Test
runs int (Test opts tree) =
    Test { opts | runs = int } tree


unit : List (() -> Test) -> Test
unit tests =
    Test { onFail = Nothing, runs = 1, doShrink = False }
        <| Group
        <| List.map (\t _ -> t ()) tests


fuzz : Fuzzer a -> List (a -> Test) -> Test
fuzz (Fuzzer gen) fuzzTests =
    Test { onFail = Nothing, runs = 100, doShrink = True }
        <| FuzzGroup
        <| (flip List.map) fuzzTests
            (\fuzzTest ( seed, runs, doShrink ) ->
                let
                    genTests =
                        Random.list runs gen |> Random.map (List.map (\arg _ -> fuzzTest arg))

                    opts =
                        { onFail = Nothing, runs = runs, doShrink = doShrink }
                in
                    Random.step genTests seed |> fst |> Group |> Test opts
            )


fuzz2 : Fuzzer a -> Fuzzer b -> List (a -> b -> Test) -> Test
fuzz2 (Fuzzer genA) (Fuzzer genB) fuzzTests =
    Test { onFail = Nothing, runs = 100, doShrink = True }
        <| FuzzGroup
        <| (flip List.map) fuzzTests
            (\fuzzTest ( seed, runs, doShrink ) ->
                let
                    genTuple =
                        Random.map2 (,) genA genB

                    genTests =
                        Random.list runs genTuple |> Random.map (List.map (\( a, b ) _ -> fuzzTest a b))

                    opts =
                        { onFail = Nothing, runs = runs, doShrink = doShrink }
                in
                    Random.step genTests seed |> fst |> Group |> Test opts
            )


assertEqual : { expected : a, actually : a } -> Test
assertEqual { expected, actually } =
    Test { onFail = Nothing, runs = 1, doShrink = False }
        <| Thunk
            (\_ ->
                if expected == actually then
                    []
                else
                    [ ( "Expected", toString expected ), ( "Actually", toString actually ) ]
            )


type ResultTree
    = Leaf (Maybe String) Outcome
    | Branch (Maybe String) (List ResultTree)


runWithSeed : Random.Seed -> Test -> ResultTree
runWithSeed seed (Test opts tree) =
    let
        unfiltered =
            case tree of
                Thunk thunk ->
                    Leaf opts.onFail (thunk ())

                Group thunks ->
                    Branch opts.onFail <| List.map (\thunk -> runWithSeed seed (thunk ())) thunks

                FuzzGroup randThunks ->
                    Random.list (List.length randThunks) Random.independentSeed
                        |> Random.map (List.map2 (\randThunk seed -> randThunk ( seed, opts.runs, opts.doShrink ) |> runWithSeed seed) randThunks)
                        |> (flip Random.step) seed
                        |> fst
                        |> Branch opts.onFail

                Batch tests ->
                    Random.list (List.length tests) Random.independentSeed
                        |> (flip Random.step) seed
                        |> fst
                        |> List.map2 (\test seed -> runWithSeed seed test) tests
                        |> Branch opts.onFail
    in
        filterSuccesses unfiltered |> Maybe.withDefault (Branch Nothing [])


filterSuccesses : ResultTree -> Maybe ResultTree
filterSuccesses rt =
    case rt of
        Leaf _ xs ->
            if List.isEmpty xs then
                Nothing
            else
                Just rt

        Branch onFail results ->
            let
                filtered =
                    List.filterMap filterSuccesses results
            in
                if List.isEmpty filtered then
                    Nothing
                else
                    Just (Branch onFail filtered)


type alias WorkerModel =
    { test : Maybe Test
    , position : Int
    }


dispatch : SupervisorModel -> ( SupervisorModel, Supervisor.Cmd )
dispatch model =
    Debug.crash "TODO implement"


decodeOutcome : Value -> Outcome
decodeOutcome =
    Debug.crash "TODO implement"


applyOutcome : Outcome -> ResultTree -> ResultTree
applyOutcome outcome result =
    Debug.crash "TODO implement"


type SupervisorMsg
    = Dispatch


updateSupervisor : SupervisorMsg -> SupervisorModel -> ( SupervisorModel, Supervisor.Cmd )
updateSupervisor supervisorMsg model =
    case supervisorMsg of
        FromWorker workerId data ->
            case Decode.decodeValue decodeOutcome data of
                Ok outcome ->
                    let
                        newResult =
                            applyOutcome model.result outcome

                        newPending =
                            Dict.remove workerId model.pending

                        newModel =
                            { model | result = newResult, pending = newPending }
                    in
                        if Dict.isEmpty newPending then
                            ( newModel, Supervisor.Cmd.Terminate )
                        else
                            dispatch newModel

                Err err ->
                    ( model, Supervisor.emit (Encode.string ("worker[" ++ workerId ++ "] sent malformed example data:" ++ toString data)) )

        FromOutside data ->
            case Decode.decodeValue supervisorMsgDecoder data of
                Ok Dispatch ->
                    dispatch model

                Err err ->
                    ( model, Supervisor.emit (Encode.string ("Error decoding message; error was: " ++ err)) )



-- WORKER --


sendError : String -> Worker.Cmd
sendError err =
    Debug.crash "TODO send error"


sendOutcome : Outcome -> Worker.Cmd
sendOutcome outcome =
    Debug.crash "TODO send outcome"


type WorkerMsg
    = RunAtPosition Int Random.Seed


workerMsgDecoder : Decoder WorkerMsg
workerMsgDecoder =
    Debug.crash "TODO implement workerMsgDecoder"


updateWorker : Value -> WorkerModel -> ( WorkerModel, Worker.Cmd )
updateWorker data model =
    case Decode.decodeValue workerMsgDecoder data of
        Ok ( RunAtPosition position, seed ) ->
            case model.test of
                Nothing ->
                    ( model
                    , sendError "Can’t run a test when there are no tests left!"
                    )

                Just test ->
                    case skipAndRun (position - model.position) seed test of
                        Ok ( resultTest, outcome ) ->
                            ( { model | test = resultTest, position = position }
                            , sendOutcome outcome
                            )

                        Err err ->
                            ( model, sendError err )

        Err err ->
            ( model, sendError err )


skipAndRun : Int -> Random.Seed -> Test -> Result String ( Maybe Test, Outcome )
skipAndRun skips seed (Test opts tree) =
    case tree of
        Thunk thunk ->
            if skips == 0 then
                Ok ( Nothing, thunk () )
            else
                Err ("Still had " ++ toString skips ++ " skip(s) left but only a Thunk remaining.")

        Group thunks ->
            case List.drop skips thunks of
                [] ->
                    Err ("After skipping " ++ toString skips ++ " items, this Group of " ++ toString (List.length thunks) ++ " was empty, but we still needed to run something.")

                thunk :: [] ->
                    Ok ( Nothing, thunk () )

                thunk :: rest ->
                    Ok ( Just (Group rest), thunk () )

        FuzzGroup randThunks ->
            Debug.crash "TODO handle FuzzGroup"

        --Random.list (List.length randThunks) Random.independentSeed
        --    |> Random.map (List.map2 (\randThunk seed -> randThunk ( seed, opts.runs, opts.doShrink ) |> runWithSeed seed) randThunks)
        --    |> (flip Random.step) seed
        --    |> fst
        --    |> Branch opts.onFail
        Batch tests ->
            Debug.crash "TODO handle Batch"



--Random.list (List.length tests) Random.independentSeed
--    |> (flip Random.step) seed
--    |> fst
--    |> List.map2 (\test seed -> runWithSeed seed test) tests
--    |> Branch opts.onFail
--type RunPath
--    = Run (Maybe String) (() -> Outcome)
--    | PathBranch (Maybe String) (List RunPath)
--prepare : Random.Seed -> Test -> Dict RunId (() -> ( Path, Outcome ))
--prepare seed (Test opts tree) =
--    case tree of
--        Thunk thunk ->
--            Run opts.onFail thunk
--        Group thunks ->
--            thunks
--                |> List.map (prepare seed)
--                |> PathBranch opts.onFail
--doThing randThunk seed _ =
--    randThunk ( seed, opts.runs, opts.doShrink )
--        |> runWithSeed seed
--        FuzzGroup randThunks ->
--            Random.independentSeed
--                |> Random.list (List.length randThunks)
--                |> Random.map (List.map2 toThing randThunks)
--                |> (flip Random.step) seed
--                |> fst
--                |> Branch opts.onFail
--        Batch tests ->
--            Random.list (List.length tests) Random.independentSeed
--                |> (flip Random.step) seed
--                |> fst
--                |> List.map2 (\test seed -> runWithSeed seed test) tests
--                |> Branch opts.onFail
