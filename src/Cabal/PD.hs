{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Copyright:  Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-3.0-or-later
--
module Cabal.PD
    ( runParseGenericPackageDescription
    , getApiModules
    , getTools
    , getLibDeps
    , pruneManFlagsGPD
    ) where

import           Utils


import           Distribution.ModuleName                      as C
import           Distribution.Package                         as C
import           Distribution.PackageDescription              as C
import           Distribution.PackageDescription.Parsec       as C
import           Distribution.Parsec.Class                    as C
import           Distribution.Parsec.Common                   as C
import           Distribution.Parsec.Field                    as C
import           Distribution.Parsec.Parser                   as C
import           Distribution.Pretty                          as C
import qualified Distribution.Text                            as C
import qualified Distribution.Types.CondTree                  as C
import qualified Distribution.Types.GenericPackageDescription as C
import           Distribution.Types.ModuleReexport            as C
import qualified Distribution.Types.UnqualComponentName       as C
import           Distribution.Verbosity                       as C
import           Distribution.Version                         as C

import qualified Data.ByteString                              as BS
import qualified Data.Map.Strict                              as Map
import qualified Data.Set                                     as Set
import qualified Data.Text                                    as T

-- | Convenience wrapper
runParseGenericPackageDescription :: ByteString -> Either (Maybe Version, [PError]) GenericPackageDescription
runParseGenericPackageDescription = snd . runParseResult . parseGenericPackageDescription

-- | Get @exposed-modules@ and @reexported-modules@
--
-- - Currently doesn't support public sub-libs
-- - Currently includes @buildable: False@ components
getApiModules :: GenericPackageDescription -> [ModuleN]
getApiModules gpd
  | Just l <- condLibrary gpd
  = let l' = fst $ C.ignoreConditions l
    in map (ModuleN . tdisp) $ nubSort $ exposedModules l' ++ map moduleReexportName (reexportedModules l')
  | otherwise = []

-- | Get executables eligible for @build-tool-depends@
--
-- Currently includes @buildable: False@ components
getTools :: GenericPackageDescription -> [ToolN]
getTools = map (ToolN . T.pack . C.unUnqualComponentName . fst) . condExecutables

-- | Get packages depended upon
--
-- - Currently ignores benchmark/testsuites
-- - Currently includes deps from @buildable: False@ components
getLibDeps :: GenericPackageDescription -> [PkgN]
getLibDeps gpd0 = map (PkgN . tdisp) $ Set.toList (alldeps Set.\\ lnames)
  where
    gpd = pruneManFlagsGPD gpd0

    alldeps = Set.fromList $ map depPkgName $ concat
      [ maybe [] (snd . C.ignoreConditions) (condLibrary gpd)
      , concatMap (snd . C.ignoreConditions . snd) (condSubLibraries gpd)
      , concatMap (snd . C.ignoreConditions . snd) (condForeignLibs gpd)
      , concatMap (snd . C.ignoreConditions . snd) (condExecutables gpd)
      ]

    lnames :: Set PackageName
    lnames = Set.fromList $ packageName gpd : map (C.unqualComponentNameToPackageName . fst) (condSubLibraries gpd)

{-

e.g. CondTree ConfVar [Dependency] Library)

where

data CondTree v c a = CondNode
    { condTreeData        :: a
    , condTreeConstraints :: c
    , condTreeComponents  :: [CondBranch v c a]
    }
    deriving (Show, Eq, Typeable, Data, Generic, Functor, Foldable, Traversable)

data CondBranch v c a = CondBranch
    { condBranchCondition :: Condition v
    , condBranchIfTrue    :: CondTree v c a
    , condBranchIfFalse   :: Maybe (CondTree v c a)
    }
    deriving (Show, Eq, Typeable, Data, Generic, Functor, Traversable)

data Condition c = Var c
                 | Lit Bool
                 | CNot (Condition c)
                 | COr (Condition c) (Condition c)
                 | CAnd (Condition c) (Condition c)
    deriving (Show, Eq, Typeable, Data, Generic)

-}


-- | Simplify & prune GPD conditionals based on manual flag defaults
pruneManFlagsGPD :: GenericPackageDescription -> GenericPackageDescription
pruneManFlagsGPD gpd
    = gpd { condLibrary      = fmap       (pruneCondTree evalCVC)  (condLibrary      gpd)
          , condSubLibraries = fmap (fmap (pruneCondTree evalCVC)) (condSubLibraries gpd)
          , condForeignLibs  = fmap (fmap (pruneCondTree evalCVC)) (condForeignLibs  gpd)
          , condExecutables  = fmap (fmap (pruneCondTree evalCVC)) (condExecutables  gpd)
          , condTestSuites   = fmap (fmap (pruneCondTree evalCVC)) (condTestSuites   gpd)
          , condBenchmarks   = fmap (fmap (pruneCondTree evalCVC)) (condBenchmarks   gpd)
          }
  where
    evalCVC :: Condition ConfVar -> Maybe Bool
    evalCVC c = evaluateCondition c evalCV

    evalCV :: C.ConfVar -> Maybe Bool
    evalCV (C.OS _)     = Nothing
    evalCV (C.Arch _)   = Nothing
    evalCV (C.Impl _ _) = Nothing
    evalCV (C.Flag fn)  = Map.lookup fn fm

    fm :: Map.Map C.FlagName Bool
    fm = Map.fromList [ (C.flagName f, C.flagDefault f) | f <- C.genPackageFlags gpd , C.flagManual f ]



pruneCondTree :: (Monoid a, Monoid d) => (C.Condition v -> Maybe Bool) -> C.CondTree v d a -> C.CondTree v d a
pruneCondTree eval (C.CondNode dat cnstr comps) = C.CondNode dat cnstr (map (pruneCondBranch eval) comps)

pruneCondBranch :: (Monoid a, Monoid d) => (C.Condition v -> Maybe Bool) -> C.CondBranch v d a -> C.CondBranch v d a
pruneCondBranch eval condb@(C.CondBranch cond iftrue miffalse)
    = case eval cond of
        Nothing   -> condb
        Just True -> C.CondBranch (C.Lit True) (pruneCondTree eval iftrue) Nothing
        Just False -> C.CondBranch (C.Lit False) emptyCondTree (fmap (pruneCondTree eval) miffalse)
  where
    emptyCondTree = C.CondNode mempty mempty mempty

-- | Ternary logic style partial evaluation
evaluateCondition :: C.Condition v
                  -> (v -> Maybe Bool) -- ^ (partial) variable assignment
                  -> Maybe Bool
evaluateCondition c0 f = walk c0
  where
    walk (C.Var v)     = f v
    walk (C.Lit b)     = Just b
    walk (C.CNot cond) = fmap not (walk cond)
    walk (C.COr cond1 cond2) = case (walk cond1, walk cond2) of
      (Just True, _)           -> Just True
      (_, Just True)           -> Just True
      (Just False, Just False) -> Just False
      (_, _)                   -> Nothing
    walk (C.CAnd cond1 cond2) = case (walk cond1, walk cond2) of
      (Just False, _)        -> Just False
      (_, Just False)        -> Just False
      (Just True, Just True) -> Just True
      (_, _)                 -> Nothing


