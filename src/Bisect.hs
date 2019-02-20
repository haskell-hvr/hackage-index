{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-3.0-or-later
--
module Bisect where

import           Utils

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           System.Exit     (ExitCode (..))
import           System.Process  (rawSystem)

import           PkgIdxTs

data TestCmdRes = TestCmdSkip | TestCmdBad | TestCmdGood
                deriving Eq

showTestCmdRes :: TestCmdRes -> [Char]
showTestCmdRes = \case
  TestCmdSkip -> "SKIP"
  TestCmdGood -> "GOOD"
  TestCmdBad  -> "BAD"

-- | Invoke test-program and map exit-codes to the respective 'TestCmdRes'
--
-- The the index-state argument is passed as last argument after the
-- extra arguments on the commandline
runTestCmd :: FilePath -> [String] -> PkgIdxTs -> IO TestCmdRes
runTestCmd cmd xargs ts = do
  ec <- rawSystem cmd (xargs ++ [fmtPkgIdxTs ts])

  case ec of
    ExitSuccess -> pure TestCmdGood
    ExitFailure n
      | n == 125 -> pure TestCmdSkip
      | n >= 1 && n <= 127 -> pure TestCmdBad
      | otherwise -> fail ("bisect script had exit-code = " ++ show n)

data BisectState
    = BisectNeedTest !Int
    | BisectDone !Int !Int

-- | Simple bisection search algorithm
--
-- Input: test samples collected so far (need at least one bad, and one good)
-- Output: next data-point to test or final result
bisectStep :: Map Int TestCmdRes -> BisectState
bisectStep evidence
  | minBadIx <= maxGoodIx = error "bisectStep: invalid argument"
  | null nextIxs = BisectDone maxGoodIx minBadIx
  | otherwise    = BisectNeedTest $! Set.elemAt (Set.size nextIxs `quot` 2) nextIxs -- TODO: try to jump out of SKIP-areas
  where
    maxGoodIx = Set.findMax goodIxs
    minBadIx  = Set.findMin badIxs
    nextIxs   = Set.fromList [ succ maxGoodIx .. pred minBadIx ] Set.\\ skipIxs

    goodIxs = Map.keysSet $ Map.filter (== TestCmdGood) evidence
    badIxs  = Map.keysSet $ Map.filter (== TestCmdBad)  evidence
    skipIxs = Map.keysSet $ Map.filter (== TestCmdSkip) evidence

