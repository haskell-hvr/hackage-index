{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Copyright: Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-3.0-or-later
--
module Main (main) where

import           Utils

import qualified Data.ByteString     as BS
import qualified Data.List           as List
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Data.Vector.Unboxed as VU
import qualified Data.Version        as V
import           Options.Applicative as OA
import qualified Paths_hackage_index
import           System.IO           (hSetBinaryMode)
import           System.Path.IO

import           Bisect
import           Cabal.Config
import           CacheDb
import           HIX
import           IndexTar
import           PkgIdxTs

----------------------------------------------------------------------------
-- CLI Interface

data Options = Options
    { optVerbose :: !Bool
    , optNoSync  :: !Bool
    , optCommand :: !Command
    }

data LogOptions = LogOptions
    { logOptLimit      :: !Int
    , logOptReverse    :: !Bool
    , logOptNoMetaRevs :: !Bool
    , logOptNoTarRevs  :: !Bool
    , logOptNoPrefs    :: !Bool
    , logOptNoR0       :: !Bool
    , logOptShowSyn    :: !Bool
    , logOptUsers      :: [UserN]
    , logOptRange      :: TsRange
    , logOptPkgN       :: [PkgN]
    }

data Sha256SumOptions = Sha256SumOptions
    { optFlatStyle :: Bool
    , optBaseDir   :: Maybe FilePath
    }

data Command
    = Sha256Sum    !Sha256SumOptions
    | Sync
    | Log          !LogOptions
    | TsParse      !TsRef
    | TsList       !TsRange
    | ProvidesMod  !VerSetStyle !ModuleN
    | ProvidesTool !VerSetStyle !ToolN
    | RDependsLib  !VerSetStyle !PkgN
    | BisectRun    !TsRef !TsRef !FilePath [String]
    | CatPD        !PkgIdR

data VerSetStyle
    = VerSetCaret2 -- concise cabal 2.0 syntax
    | VerSetCaret3 -- concise cabal 3.0 syntax
    | VerSetCaret3Explicit -- explicit single-point pseudo cabal 3.0 syntax

instance Reader VerSetStyle where
  readm = OA.eitherReader $ \s -> case lookup s tokens of
      Just t -> pure t
      Nothing -> Left ("unknown style " ++ show s ++ " (must be one of " ++ (unwords (map (show . fst) tokens)) ++ ")")
    where
      tokens = [ ("cabal2", VerSetCaret2)
               , ("cabal3", VerSetCaret3)
               , ("cabal3x", VerSetCaret3Explicit)
               ]


optionsParserInfo :: OA.ParserInfo Options
optionsParserInfo
    = info (helper <*> verOption <*> oParser)
           (fullDesc
            <> header "hit - Hackage index tool"
            <> footer "\
              \ Each command has a sub-`--help` text. \
              \ This tool reads the `~/.cabal/config` configuration file and honors the `CABAL_CONFIG` environemnt variable.\
              \ TSREF := [ '@' | ISOTS | PKGID-REV ], TSRANGE := TSREF [ '..' [ TSREF ] ]. ISOTS := e.g. '2014-11-22T12:39:07Z'.\
              \ PKGID-REV := e.g. 'lens-1.2.3-r4'. \
              \")

  where
    indexssParser = Sha256Sum <$> (Sha256SumOptions <$> switch (long "flat" <> help "flat filesystem layout (used by mirrors)")
                                                        <*> optional (OA.argument str (metavar "BASEDIR")))


    logCmdParser = Log <$> (
      LogOptions <$> OA.option auto ( OA.short 'n' <> OA.long "max-count" <> OA.value (-1) <> OA.metavar "NUM" <>
                                      OA.help "Limit the number of index entries printed")
                 <*> switch (long "reverse"      <> OA.help "Output older index entries first")
                 <*> switch (long "no-revisions" <> OA.help "No metadata-only revisions")
                 <*> switch (long "no-uploads"   <> OA.help "No source-dist uploads")
                 <*> switch (long "no-preferred" <> OA.help "No preferred-versions changes")
                 <*> switch (long "no-r0-tag"    <> OA.help "Suppress printing '-r0' tag for 0th revision")
                 <*> switch (long "show-synopsis" <> OA.help "Show synopsis for each revision/upload")
                 <*> many (UserN <$> OA.strOption ( OA.long "user" <> metavar "USERNAME" <> OA.help "Filter on user name; use multiple times for disjunction"))
                 <*> OA.argument readm (metavar "TSRANGE" <> OA.help "limit by listing by timestamps; TSRANGE := TSREF [ '..' [ TSREF ] ]  (default: @)" <> value (TsRange TsRef0 TsRefLatest))
                 <*> many (OA.argument readm (metavar "PKG-NAME..." <> OA.help "Filter output to set of package names"))
      )

    oParser = Options <$> switch (long "verbose" <> help "enable verbose output")
                      <*> switch (long "no-sync" <> help "suppress automatic cache syncing against 01-index.tar")
                      <*> subparser (mconcat subcmds)

    subcmds = [ command "sha256sum" (info (helper <*> indexssParser)
                                      (progDesc "generate sha256sum-format file"))

              , command "sync" (info (helper <*> pure Sync)
                               (progDesc "sync index cache"))

              , command "cat-pd" (info (helper <*> (CatPD <$> OA.argument readm (metavar "PKGID-REV")))
                               (progDesc "Dump package description to stdout"))

              , command "log" (info (helper <*> logCmdParser)
                               (progDesc "show package index changelog"))
              , command "ts-parse" (info (helper <*> (TsParse <$> OA.argument readm (metavar "TSREF")))
                               (progDesc "resolve reference to timestamp suitable as --index-state argument"))
              , command "ts-list" (info (helper <*> (TsList <$> OA.argument readm (metavar "TSRANGE")))
                               (progDesc "resolve reference to timestamp suitable as --index-state argument"))
              , command "provides-module" (info (helper <*> (ProvidesMod
                                                             <$> vstyleOpt
                                                             <*> OA.argument readm (metavar "MODULE-NAME")))
                               (progDesc "list packages potentially exposing/reexporting a given module"))
              , command "provides-tool" (info (helper <*> (ProvidesTool
                                                             <$> vstyleOpt
                                                             <*> OA.argument readm (metavar "TOOL-NAME")))
                               (progDesc "list packages potentially providing a given tool/executable component"))
              , command "rdepends" (info (helper <*> (RDependsLib
                                                       <$> vstyleOpt
                                                       <*> OA.argument readm (metavar "PKG-NAME")))
                               (progDesc "list packages potentially having a library-component dependency on given package-name"))

              , command "bisect-run" (info (helper <*> (BisectRun
                                                       <$> OA.argument readm (metavar "TSREF1")
                                                       <*> OA.argument readm (metavar "TSREF2")
                                                       <*> OA.strArgument (metavar "CMD"))
                                                       <*> many (OA.strArgument (metavar "CMDARG..."))
                                           )
                               (progDesc "Perform binary search (in the style of 'git bisect run') between TSREF1 and TSREF2 to find index-state which introduced a change"
                                <> footer "The exit code of CMD determines whether an index-state is considered as GOOD (0), BAD (1..124,126..127), or SKIP (125)."
                               ))
              ]

    vstyleOpt = OA.option readm (long "vstyle" <> help "version set formatting style" <> value VerSetCaret2)

    verOption = infoOption verMsg (long "version" <> help "output version information and exit")
      where
        verMsg = "hit " <> V.showVersion Paths_hackage_index.version

----------------------------------------------------------------------------

main :: IO ()
main = do
    opts <- execParser optionsParserInfo
    mainWithOptions opts

mainWithOptions :: Options -> IO ()
mainWithOptions Options {..} = do
     case optCommand of
       Sha256Sum opts      -> doSha256Sum opts
       Sync                -> doSync
       Log opts            -> doLog opts
       TsParse tsref       -> doTsParse tsref
       TsList tsref        -> doTsList tsref
       ProvidesMod x m     -> doProvidesMod x m
       ProvidesTool x m    -> doProvidesTool x m
       RDependsLib x m     -> doRDependsLib x m
       BisectRun t1 t2 c x -> doBisectRun t1 t2 c x
       CatPD pidr          -> doCatPD pidr
     return ()
  where
    doSync :: IO ()
    doSync = withCacheDb optNoSync (pure ())

    doSha256Sum Sha256SumOptions{..} = withCacheDb optNoSync $ do
      rows <- dbQuery_ "SELECT pname,ver,hex(tarsha) FROM pkgids_str NATURAL JOIN tars WHERE srev=0 ORDER BY pname,ver"
      forM_ rows $ \(pn,pv,sha) -> do
        liftIO $ T.putStrLn $ if optFlatStyle
                              then mconcat [ sha, "  ", maybe "" T.pack optBaseDir, pn, "-", pv, ".tar.gz" ]
                              else mconcat [ sha, "  ", maybe "" T.pack optBaseDir, pn, "/", pv, "/", pn, "-", pv, ".tar.gz" ]
      pure ()

    ----------------------------------------------------------------------------

    doCatPD :: PkgIdR -> IO ()
    doCatPD pidr = do
      (_, ofs, cabsha) <- withCacheDb optNoSync $ resolvePkgIdR pidr
      ifn <- getIndexTarFn hackageRepoId
      cabBs <- withFile ifn ReadMode $ \h -> do
        hSetBinaryMode h True
        readTarNormalFile1 h ofs
      unless (sha256hash cabBs == cabsha) $ fail "internal integrity failure (.cabal SHA256 mismatch)"
      BS.putStr cabBs

    doLog :: LogOptions -> IO ()
    doLog LogOptions{..} = withCacheDb optNoSync $ do
      let TsRange tref1 tref2 = logOptRange

      t1 <- resolveTsRef tref1
      t2 <- resolveTsRef tref2

      -- TODO: Maybe warn if (logOptNoPrefs && logOptNoTarRevs && logOptNoMetaRevs)?

      -- --user
      dbExecute_ "DROP TABLE IF EXISTS tmp_t1"
      dbExecute_  "CREATE TEMPORARY TABLE tmp_t1 (uname_id INTEGER NOT NULL)"
      forM_ logOptUsers $ \x -> do
        res <- dbQuery "SELECT uname_id FROM unames WHERE uname = ?" (Only x)
        case res of
          []  -> fail ("user " ++ show x ++ " not found")
          [y] -> dbExecute "INSERT INTO tmp_t1(uname_id) VALUES(?)" (y :: Only Int)
          (_:_:_) -> fail "impossible"

      -- --package
      dbExecute_ "DROP TABLE IF EXISTS tmp_t2"
      dbExecute_  "CREATE TEMPORARY TABLE tmp_t2 (pname_id INTEGER NOT NULL)"
      forM_ logOptPkgN $ \x -> do
        res <- dbQuery "SELECT pname_id FROM pnames WHERE pname = ?" (Only x)
        case res of
          []  -> fail ("package '" ++ (\(PkgN s) -> T.unpack s) x ++ "' not found")
          [y] -> dbExecute "INSERT INTO tmp_t2(pname_id) VALUES(?)" (y :: Only Int)
          (_:_:_) -> fail "impossible"

      -- constructing this query would benefit from having a SQL EDSL...
      rows <- dbQuery (mconcat
              [ "SELECT ts,pname,ver,rev,uname,"
              , if logOptShowSyn then "(SELECT syn FROM syns WHERE syns.syn_id = r.syn_id)" else "''"
              , "  FROM revisions r, pkgids_str s, unames u, pkgids p \
                \  WHERE r.pkgid = s.pkgid AND r.uname_id = u.uname_id AND r.pkgid = p.pkgid \
                \        AND r.ts BETWEEN ? AND ? \
                \        AND (CASE (select count(*) from tmp_t1) WHEN 0 THEN 1 ELSE r.uname_id in (select uname_id from tmp_t1) END) \
                \        AND (CASE (select count(*) from tmp_t2) WHEN 0 THEN 1 ELSE p.pname_id in (select pname_id from tmp_t2) END)"
              , if logOptNoMetaRevs then " AND r.rev = 0 " else ""
              , if logOptNoTarRevs  then " AND r.rev > 0 " else ""
              , "  ORDER BY r.ts"
              , if logOptReverse then " ASC " else " DESC "
              , " LIMIT ?"
              ]) (t1, t2, (if logOptLimit > 0 then logOptLimit else 0xffffffff {- ugly hack -}))

      rows2 <- if logOptNoPrefs then pure mempty else
               dbQuery (mconcat
               [ "SELECT ts,pname FROM prefs pr JOIN pnames pn USING (pname_id) WHERE "
               , " ts BETWEEN ? AND ? "
               , " AND (CASE (select count(*) from tmp_t2) WHEN 0 THEN 1 ELSE pr.pname_id in (select pname_id from tmp_t2) END)"
               , "  ORDER BY ts"
               , if logOptReverse then " ASC " else " DESC "
               , " LIMIT ?"
               ]) (t1, t2, (if logOptLimit > 0 then logOptLimit else 0xffffffff {- ugly hack -}))

      -- mergesort
      let go [] [] = []
          go [] rs2 = map Right rs2
          go rs1 [] = map Left rs1
          go rs1@(r1@(ts1,_,_,_,_,_):rows1') rs2@(r2@(ts2,_):rows2')
            | if logOptReverse then ts1 > ts2 else ts1 < ts2
            = Right r2 : go rs1 rows2'
            | otherwise
            = Left r1 : go rows1' rs2

      let fmtPkgId' pn pv prev
            | logOptNoR0, prev == PkgR 0  = fmtPkgId  (PkgId  pn pv)
            | otherwise                   = fmtPkgIdR (PkgIdR pn pv prev)

      liftIO $ forM_ ((if logOptLimit > 0 then take logOptLimit else id) (go rows rows2)) $ \x -> do
        case x of
          Left (ts,pn,pv,prev,uname,syn)
            | logOptShowSyn -> T.putStrLn (T.pack $ mconcat [ fmtPkgIdxTs ts, "\t", uname , "\t", fmtPkgId' pn pv prev, "\t", quoteSyn syn ])
            | otherwise     -> T.putStrLn (T.pack $ mconcat [ fmtPkgIdxTs ts, "\t", uname , "\t", fmtPkgId' pn pv prev ])
          Right (ts,pn)
            | logOptShowSyn -> T.putStrLn (T.pack $ mconcat [ fmtPkgIdxTs ts, "\t-\t", pn, "\t-" ])
            | otherwise     -> T.putStrLn (T.pack $ mconcat [ fmtPkgIdxTs ts, "\t-\t", pn ])

      pure ()

    ----------------------------------------------------------------------------

    doTsParse :: TsRef -> IO ()
    doTsParse tsref = withCacheDb optNoSync $ do
      resolveTsRef tsref >>= \case
        Nothing -> liftIO exitFailure
        Just t' -> liftIO (putStrLn (fmtPkgIdxTs t'))

    ----------------------------------------------------------------------------

    doTsList :: TsRange -> IO ()
    doTsList (TsRange tref1 tref2) = withCacheDb optNoSync $ do
      t1 <- resolveTsRef tref1
      t2 <- resolveTsRef tref2

      tslst <- fmap (\(Only x) -> x) <$> dbQuery "SELECT ts FROM revisions WHERE ts BETWEEN ? AND ? UNION SELECT ts FROM prefs WHERE ts BETWEEN ? AND ? ORDER BY ts DESC" (t1, t2, t1, t2)
      liftIO $ forM_ tslst (T.putStrLn . T.pack . fmtPkgIdxTs)

    ----------------------------------------------------------------------------

    doProvidesMod vstyle mname = doProvides vstyle $
      dbQuery "SELECT s.pname,s.ver \
              \  FROM pkgids_mnames pm, mnames m, pkgids_str s \
              \  WHERE mname = ? AND m.mname_id = pm.mname_id AND s.pkgid = pm.pkgid" (Only mname)

    doProvidesTool vstyle tname = doProvides vstyle $
      dbQuery "SELECT s.pname,s.ver \
              \  FROM pkgids_tools pm, tools m, pkgids_str s \
              \  WHERE tool = ? AND m.tool_id = pm.tool_id AND s.pkgid = pm.pkgid" (Only tname)

    doRDependsLib vstyle pname = doProvides vstyle $
      dbQuery "SELECT s.pname,s.ver \
              \  FROM pkgids_deps d JOIN pkgids_str s USING (pkgid) \
              \  WHERE d.pname_id = (SELECT pname_id FROM pnames WHERE pname = ?)" (Only pname)

    doProvides vstyle qry = withCacheDb optNoSync $ do
      rows <- qry

      let p2vs = Map.fromListWith mappend [ (k, Set.singleton (v :: V)) | (k,v) <- rows ]

      forM_ (Map.toList p2vs) $ \(pname, pvers) -> do
        pvers00 <- map (\(Only x) -> (x::V)) <$> dbQuery "SELECT ver FROM pnames n, pkgids pi, vers v \
                                                         \WHERE pname = ? AND n.pname_id = pi.pname_id AND pi.ver_id = v.ver_id \
                                                         \" (Only pname)

        let pvers0 = Set.fromList pvers00
            npvers = (pvers0 Set.\\ pvers)
            allvers = Set.map (\x -> (x,True)) pvers <> Set.map (\x -> (x,False)) npvers
            gvers = List.groupBy (\x y -> snd x == snd y) $ Set.toList allvers

        case vstyle of
          VerSetCaret3Explicit -> liftIO $ do
            forM_ gvers $ \vs@((_,isIn):_) -> do
              T.putStrLn (pname <> (if isIn then " == { " else " /= { ") <> T.intercalate ", " (map tdisp (map fst vs)) <> " }")

          VerSetCaret2 -> liftIO $ do
            T.putStrLn (pname <> "  " <> T.intercalate " || "
                        [ case mub of { Nothing -> "^>= " <> tdisp lb
                                      ; Just ub -> "(^>= " <> tdisp lb <> " && < " <> tdisp (vstrip0s ub) <> ")"
                                      }
                        | (lb,mub) <- groupCaret allvers ])

          VerSetCaret3 -> liftIO $ do
            let points = groupCaret allvers
                points1 = [ lb | (lb,Nothing) <- points ]

            T.putStrLn (pname <> "  " <>
                        T.intercalate " || " (
                        (if null points1 then [] else ["^>= { " <> T.intercalate ", " (map tdisp points1) <> " }"])
                         ++ [ "(^>= " <> tdisp lb <> " && < " <> tdisp (vstrip0s ub) <> ")" | (lb,Just ub) <- points ]))

        liftIO $ putStrLn ""

    doBisectRun :: TsRef -> TsRef -> FilePath -> [FilePath] -> IO ()
    doBisectRun tref1 tref2 testcmd xargs = do

      tslist <- withCacheDb optNoSync $ do
        t1 <- resolveTsRef tref1
        t2 <- resolveTsRef tref2

        fmap (\(Only x) -> x :: PkgIdxTs) <$> dbQuery "SELECT ts FROM revisions WHERE ts BETWEEN ? AND ? UNION SELECT ts FROM prefs WHERE ts BETWEEN ? AND ? ORDER BY ts ASC" (t1, t2, t1, t2)

      let tsvec = VU.fromList tslist

      logInfo (mconcat [ "Performing binary search between " <> fmtPkgIdxTs (VU.head tsvec) <> " and " <> fmtPkgIdxTs (VU.last tsvec) <> " (" <> show (VU.length tsvec) <> " index-states)" ])

      let go samples = do
            case bisectStep samples of
              BisectNeedTest mi -> do
                let mits = tsvec VU.! mi
                rc <- runTestCmd testcmd xargs mits
                logInfo (fmtPkgIdxTs mits <> " -> " <> showTestCmdRes rc)
                go (Map.insert mi rc samples)
              BisectDone li ui -> do
                logInfo ("bisection completed; the change was introduced in index-state " <> fmtPkgIdxTs (tsvec VU.! ui)
                          <> (" (log " <> fmtPkgIdxTs (tsvec VU.! li) <> ".." <> fmtPkgIdxTs (tsvec VU.! ui) <> " )"))

                -- print collected evidence
                T.putStrLn ""
                forM_ (Map.toList samples) $ \(mi, st) ->
                  T.putStrLn $ T.pack $ mconcat [ show mi, "\t", fmtPkgIdxTs (tsvec VU.! mi), "\t"
                                                , showTestCmdRes st
                                                , if ui == mi then "\t<<<" else if li == mi then "\t>>>" else "" ]
                T.putStrLn ""

      -- start the bisection
      go (Map.fromList [(0,TestCmdGood),(VU.length tsvec-1,TestCmdBad)])

    ----------------------------------------------------------------------------

-- | Resolve a TSREF to a index-state value
--
-- Returns 'Nothing' if timestamp out of range; throws exception if invalid pkg-id-ref is used
resolveTsRef :: TsRef -> HIX (Maybe PkgIdxTs)
resolveTsRef TsRefLatest = do
  (mts1,mts2) <- dbQuery1_ "SELECT (SELECT max(ts) FROM revisions), (SELECT max(ts) FROM prefs)"
  case onMaybes max mts1 mts2 of
    Nothing -> pure Nothing
    Just t' -> pure (Just t')
resolveTsRef TsRef0 = do
  (mts1,mts2) <- dbQuery1_ "SELECT (SELECT min(ts) FROM revisions), (SELECT min(ts) FROM prefs)"
  case onMaybes min mts1 mts2 of
    Nothing -> pure Nothing
    Just t' -> pure (Just t')
resolveTsRef (TsRefTs t0) = do
  (mts1,mts2) <- dbQuery1 "SELECT (SELECT max(ts) FROM revisions WHERE ts <= ?), (SELECT max(ts) FROM prefs WHERE ts <= ?)" (t0, t0)
  pure $ onMaybes max mts1 mts2
resolveTsRef (TsRefPkgId pidr) = do
  (ts, _, _) <- resolvePkgIdR pidr
  pure (Just ts)


resolvePkgIdR :: PkgIdR -> HIX (PkgIdxTs, Word32, SHA256Val)
resolvePkgIdR (PkgIdR pn pv pr) = do
    pn_key <- do res <- dbQuery "SELECT pname_id FROM pnames WHERE pname = ?" (Only pn)
                 case res of
                   []       -> fail ("package '" ++ pnstr ++ "' not found")
                   [Only k] -> pure k
                   (_:_:_)  -> fail "internal inconsistency"

    pid_key <- do res <- dbQuery "SELECT pkgid FROM pkgids JOIN vers USING (ver_id) WHERE pname_id = ? and ver = ?" (pn_key :: Int, pv)
                  case res of
                   [] -> fail ("package '" ++ pnstr ++ "' has no version " ++ pvstr)
                   [Only k] -> pure k
                   (_:_:_) -> fail "internal inconsistency"

    revdata <- do res <- dbQuery "SELECT ts,ofs,cabsha FROM revisions WHERE pkgid = ? and rev = ?" (pid_key :: Int, pr)
                  case res of
                   [] -> fail ("release '" ++ pnstr ++ "-" ++ pvstr ++ "' has no revision " ++ prstr)
                   [k] -> pure k
                   (_:_:_) -> fail "internal inconsistency"

    return revdata
  where
    pnstr = (\(PkgN s) -> T.unpack s) pn
    pvstr = (\(PkgV s) -> T.unpack s) pv
    prstr = "r" ++ show ((\(PkgR n) -> n) pr)

-- | Wraps in @"@s, normalizes whitespaces, and uses dup-escaping for @"@s
quoteSyn :: String -> String
quoteSyn s0 = '"' : go (unwords (words s0))
  where
    go []       = '"':[]
    go ('"':cs) = '"':'"':go cs
    go (c:cs)   = c:go cs
