module Suites.Suite where

{-| Tests for the `Suite` module. -}

-- TODO have a fuzz test for `withRuns` which verifies that given the same 2
-- randomly-generated seeds, another (complicated) fuzz test always produces
-- the same successes and failures (after sorting & shrinking).
