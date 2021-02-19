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

    , canUseLibDep
    , conflictsWithLibDep
    ) where

import           Types
import           Utils

import qualified Data.ByteString                              as BS
import qualified Data.Map.Strict                              as Map
import qualified Data.List.NonEmpty as NE
import qualified Data.Set                                     as Set
import qualified Data.Text                                    as T
import           Distribution.ModuleName                      as C
import           Distribution.Package                         as C
import           Distribution.PackageDescription              as C
import           Distribution.PackageDescription.Parsec       as C
import           Distribution.Parsec                          as C
import           Distribution.Pretty                          as C
import qualified Distribution.Text                            as C
import qualified Distribution.Types.CondTree                  as C
import qualified Distribution.Types.GenericPackageDescription as C
import           Distribution.Types.ModuleReexport            as C
import qualified Distribution.Types.UnqualComponentName       as C
import           Distribution.Verbosity                       as C
import           Distribution.Version                         as C

-- | Convenience wrapper
runParseGenericPackageDescription :: ByteString -> Either (Maybe Version, NE.NonEmpty PError) GenericPackageDescription
runParseGenericPackageDescription = snd . runParseResult . parseGenericPackageDescription

-- | Get @exposed-modules@ and @reexported-modules@
--
-- - Currently doesn't support public sub-libs
-- - Currently includes @buildable: False@ components
getApiModules :: GenericPackageDescription -> [ModuleN]
getApiModules gpd
  | Just l <- condLibrary gpd
  = let l' = fst $ C.ignoreConditions l
    in map mkModuleN $ nubSort $ exposedModules l' ++ map moduleReexportName (reexportedModules l')
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
getLibDeps gpd0 = map mkPkgN $ Set.toList (alldeps Set.\\ lnames)
  where
    gpd = pruneManFlagsGPD gpd0

    alldeps = Set.fromList $ map depPkgName $ concat
      [ maybe [] (snd . C.ignoreConditions) (condLibrary gpd)
      , concatMap (snd . C.ignoreConditions . snd) (condSubLibraries gpd)
      , concatMap (snd . C.ignoreConditions . snd) (condForeignLibs gpd)
      , concatMap (snd . C.ignoreConditions . snd) (condExecutables gpd)
      ]

    -- sub-lib names shadowing global names
    lnames :: Set PackageName
    lnames = Set.fromList $ packageName gpd : map (C.unqualComponentNameToPackageName . fst) (condSubLibraries gpd)


-- rangeForPackage :: GenericPackageDescription -> PackageName ->

depTreeFromCondTree :: PackageName -> CondTree v [Dependency] a -> DepTree
depTreeFromCondTree pn = go2 . C.mapTreeConstrs go
  where
    go :: [Dependency] -> Maybe VersionRange
    go ds = case [ depVerRange d | d <- ds, depPkgName d == pn ] of
              []  -> Nothing
              vrs -> Just (simplifyVersionRange (foldr1 intersectVersionRanges vrs))

    go2 :: CondTree v (Maybe VersionRange) a -> DepTree
    go2 (CondNode _ mvr branches) = DepNode mvr [ (go2 tb,fmap go2 fb) | C.CondBranch _ tb fb <- branches ]

canUseLibDep :: GenericPackageDescription -> PkgId -> Bool
canUseLibDep gpd0 (PkgId pn (PkgV v0)) = case mapMaybe (depTreeCanUseVersion v0) dts of
    [] -> False
    xs -> and xs
  where
    gpd = pruneManFlagsGPD gpd0

    pn' = mkPackageName (disp pn)

    dts :: [DepTree]
    dts = concat
      [ maybe [] (pure . depTreeFromCondTree pn') (condLibrary gpd)
      , map (depTreeFromCondTree pn' . snd) (condSubLibraries gpd)
      , map (depTreeFromCondTree pn' . snd) (condForeignLibs gpd)
      , map (depTreeFromCondTree pn' . snd) (condExecutables gpd)
      ]

conflictsWithLibDep :: GenericPackageDescription -> PkgId -> Bool
conflictsWithLibDep gpd0 (PkgId pn (PkgV v0)) = not $ all (depTreeDontConflictVersion v0) dts
  where
    gpd = pruneManFlagsGPD gpd0

    pn' = mkPackageName (disp pn)

    dts :: [DepTree]
    dts = concat
      [ maybe [] (pure . depTreeFromCondTree pn') (condLibrary gpd)
      , map (depTreeFromCondTree pn' . snd) (condSubLibraries gpd)
      , map (depTreeFromCondTree pn' . snd) (condForeignLibs gpd)
      , map (depTreeFromCondTree pn' . snd) (condExecutables gpd)
      ]

-- NOTE/TODO: This is currently a bit too dumb and can be tricked by pathological conditionals

-- simplified CondTree
data DepTree = DepNode (Maybe VersionRange) [(DepTree,Maybe DepTree)]
  deriving Show

-- there needs to be at least one actively accepting range
-- NB: subset of depTreeDontConflictVersion
--
-- Just False: actively conflicts with version
-- Just True: accepts version (i.e. there's an actual possible use)
-- Nothing: doesn't use and doesn't conflict
depTreeCanUseVersion :: C.Version -> DepTree -> Maybe Bool
depTreeCanUseVersion v0 t0 = case goNode' t0 of
  Nothing           -> Nothing
  Just (a,False)    -> Just a
  Just (True,True)  -> Just True
  Just (False,True) -> Nothing
  where
    -- |
    --
    -- Possible outcomes:
    --
    --  1. dep is captured unconditionally (i.e. by both branches) => 'Just (_,False)'
    --      a) accepting v
    --      b) rejecting v
    --  2. dep is conditionally captured => 'Just (_,True)'
    --      a) accepting v
    --      b) rejecting v  (effectively similar to 3)
    --  3. dep is not captured => 'Nothing'
    --
    goBranch' (iftrue, Nothing) =
      -- possible: 2a, 2b, 3
      case goNode' iftrue of
        Nothing    -> Nothing -- 3
        Just (a,_) -> Just (a,True) -- 2a, 2b
    goBranch' (iftrue, Just iffalse) =
      case (goNode' iftrue, goNode' iffalse) of
        (Nothing, Nothing)           -> Nothing -- 3
        (Just (a,_), Nothing)        -> Just (a, True) -- 2a, 2b
        (Nothing, Just (a,_))        -> Just (a, True) -- 2a, 2b
        (Just (a1,c1), Just (a2,c2)) -> Just (a1 || a2, c1 || c2) -- 1a,1b,2a,2b

    goNode' (DepNode (Just vr) branches)
      | withinRange v0 vr = Just (all goBranch branches,False)  -- possible: 1a, 1b
      | otherwise = Just (False, False)  -- 1b

    goNode' (DepNode Nothing branches)
      {- possible outcomes: ALL

         1. no branch may conflict
         2. there must be at least one branch capturing && accepting pkg-id

      -}

      | null branches' = Nothing -- no sub-nodes are capturing
      | otherwise = Just (isAccept, isCond)
      where
        branches' :: [(Bool,Bool)]
        branches' = mapMaybe goBranch' branches

        isCond = null unconds
        (conds,unconds) = bimap (map fst) (map fst) $ partition snd branches'

        isAccept | isCond      = or conds -- if there's no unconds, at least one cond must be accepting
                 | otherwise   = and unconds -- all uncond must be accepting; unconds don't matter anymore


    -- same as for depTreeDontConflictVersion
    goNode (DepNode mvr branches) = fromMaybe True (inVR mvr) && all goBranch branches

    goBranch (_,      Nothing)      = True
    goBranch (iftrue, Just iffalse) = goNode iftrue || goNode iffalse

    inVR Nothing   = Nothing
    inVR (Just vr) = Just $! withinRange v0 vr


depTreeDontConflictVersion :: C.Version -> DepTree -> Bool
depTreeDontConflictVersion v0 = goNode
  where
    goNode (DepNode mvr branches) = maybe True (withinRange v0) mvr && all goBranch branches

    goBranch (_,      Nothing)      = True
    goBranch (iftrue, Just iffalse) = goNode iftrue || goNode iffalse


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
    evalCV (C.PackageFlag fn)  = Map.lookup fn fm

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


