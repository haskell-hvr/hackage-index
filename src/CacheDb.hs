{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

-- |
-- Copyright: Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-3.0-or-later
--
module CacheDb (withCacheDb) where

import           Cabal.Config                                 (hackageHaskellOrg)
import qualified Codec.Archive.Tar.Entry                      as Tar
import qualified Crypto.Hash.SHA256                           as SHA256
import qualified Data.ByteString                              as BS
import qualified Data.ByteString.Lazy                         as BSL
import qualified Data.Text                                    as T
import qualified Database.SQLite.Simple                       as DB
import qualified Distribution.Text                            as C
import qualified Distribution.Types.GenericPackageDescription as C
import qualified Distribution.Types.PackageDescription        as C
import qualified Distribution.Utils.ShortText                 as C
import qualified System.FilePath                              as FP
import           System.IO                                    (hSetBinaryMode,
                                                               hTell)

import           Cabal.PD
import           HIX
import           IndexTar
import           PkgIdxTs
import           System.Path.IO
import           Types
import           Utils

type PkgIdKey = Int

hackageRepoId :: T.Text
hackageRepoId = T.pack hackageHaskellOrg

withCacheDb :: Bool -> HIX a -> IO a
withCacheDb noSync act = do
    -- putStrLn "syncing..."

    cacheDir <- (</> fragment "hackage-index") <$> getXdgCacheDirectory
    createDirectoryIfMissing True cacheDir

    let dbfn = cacheDir </> fragment "cache.db"

    mdbver0 <- getDbVer dbfn

    case mdbver0 of
      Nothing
        | noSync -> fail "Missing cache db but --no-sync prevents initialization. Aborting."
      Nothing -> do
        logInfo "Initializing new cache db. This might take a couple minutes..."
        resetDb dbfn

      Just dbver0
        | dbver0 == curDbVer -> return ()
        | otherwise -> do
            logWarn ("Unexpected cache version " ++ show dbver0 ++ " detected. Reinitializing cache. This might take a couple minutes...")
            resetDb dbfn

    DB.withConnection (toFilePath dbfn) $ \conn -> do
      enableFK conn
      [Only dbver] <- DB.query_ conn "PRAGMA user_version"
      unless (dbver == curDbVer) $ fail "unexpected user_version"

      let registerPkgId :: PkgN -> PkgV -> IO (PkgIdKey,PkgR)
          registerPkgId n v = do
            DB.execute conn "INSERT OR IGNORE INTO pnames(pname) VALUES (?)" (Only n)
            DB.execute conn "INSERT OR IGNORE INTO vers(ver) VALUES (?)"     (Only v)
            DB.execute conn "INSERT OR IGNORE INTO pkgids(pname_id,ver_id) SELECT pname_id, ver_id \
                             \  FROM pnames, vers WHERE pname = ? AND ver = ?" (n, v)

            [(pkgid,revcnt)] <- DB.query conn "SELECT i.pkgid,i.revcnt \
                                              \ FROM pkgids i, pnames p, vers v \
                                              \ WHERE i.pname_id = p.pname_id AND i.ver_id = v.ver_id AND p.pname = ? AND v.ver = ?" (n, v)

            return (pkgid,revcnt)

      let go te ofs _ofs2 mdata = do
            let fn = Tar.fromTarPathToPosixPath (Tar.entryTarPath te)
                ts = pkgIdxTsFromTarEpoch (Tar.entryTime te)
                owner = Tar.entryOwnership te
                uid = Tar.ownerId owner
            case FP.takeFileName fn of
              "package.json" -> do
                let Just bs = mdata
                    Just (IndexShaEntry tarfn h1 _h2 tarsz) = decodePkgJsonFile (BSL.fromStrict bs)
                    PkgId n v = fn2pkgid tarfn

                (pkgid,_) <- registerPkgId n v

                DB.execute conn "INSERT INTO tars(pkgid,srev,ts,ofs,tarsha,tarsz) VALUES(?,0,?,?,?,?)" (pkgid,ts,ofs,h1,tarsz)

                pure ()

              "preferred-versions" -> do
                let n = T.pack (FP.takeDirectory fn)

                DB.execute conn "INSERT OR REPLACE INTO prefs(pname_id,ts,ofs) VALUES((SELECT pname_id FROM pnames WHERE pname = ?),?,?)" (n,ts,ofs)

                return ()

              fn' | FP.takeExtension fn' == ".cabal" -> do
                      let (n0,'/':v0) = break (=='/') $ FP.takeDirectory fn
                          n = mkPkgN n0'
                          Just v = C.simpleParse v0
                          Just n0' = C.simpleParse n0

                      DB.execute conn "INSERT OR IGNORE INTO unames(uname_id,uname) VALUES (?,?)"
                                      (uid, T.pack (Tar.ownerName owner))

                      (pkgid,revcnt) <- registerPkgId n v

                      DB.execute conn "UPDATE pkgids SET revcnt = revcnt + 1 WHERE pkgid = ?" (Only (pkgid :: Int))

                      let Just cabBs' = mdata
                          cabSha = sha256hash cabBs'

                      mgpd <- case runParseGenericPackageDescription cabBs' of
                        Left (_mcv, perrs) -> do
                          logWarn ("Failed to parse .cabal for " ++ show (disp (PkgIdR n v revcnt),disp ts) ++ " : " ++ show perrs)

                          pure Nothing
                        Right gpd -> pure (Just gpd)

                      let Just gpd = mgpd
                          mods = getApiModules gpd
                          syn = T.pack $ C.fromShortText $ C.synopsis $ C.packageDescription gpd
                          tools = getTools gpd

                          libdeps = getLibDeps gpd

                      DB.execute conn "INSERT OR IGNORE INTO syns(syn) VALUES(?)" (Only syn)

                      DB.execute conn "INSERT INTO revisions(pkgid,rev,srev,ts,cabsha,uname_id,ofs,syn_id) VALUES(?,?,0,?,?,?,?,(SELECT syn_id FROM syns WHERE syn = ?))"
                                      (pkgid, revcnt, ts, cabSha, uid, ofs, syn)

                      DB.executeMany conn "INSERT OR IGNORE INTO pnames(pname) VALUES(?)" (map Only libdeps)
                      DB.executeMany conn "INSERT OR IGNORE INTO mnames(mname) VALUES(?)" (map Only mods)
                      DB.executeMany conn "INSERT OR IGNORE INTO tools(tool) VALUES(?)" (map Only tools)

                      -- NB: these need to be `OR IGNORE` because there can be more than one revision per pkgid
                      DB.executeMany conn "INSERT OR IGNORE INTO pkgids_deps(pkgid,pname_id) VALUES(?,(SELECT pname_id FROM pnames WHERE pname = ?))" [ (pkgid,x) | x <- libdeps ]
                      DB.executeMany conn "INSERT OR IGNORE INTO pkgids_mnames(pkgid,mname_id) VALUES(?,(SELECT mname_id FROM mnames WHERE mname = ?))" [ (pkgid,x) | x <- mods ]
                      DB.executeMany conn "INSERT OR IGNORE INTO pkgids_tools(pkgid,tool_id) VALUES(?,(SELECT tool_id FROM tools WHERE tool = ?))" [ (pkgid,x) | x <- tools ]

                      return ()
                  | otherwise -> fail ("Unexpected index entry " ++ show fn)

            -- putStrLn ("inserting " ++ show fn)

            return ()

      mrepo0 <- DB.query conn "SELECT repo_id,idxhash,blocks FROM repos WHERE repo_name = ?" (Only hackageRepoId)
      let (mrepoId, oldIdxHash, oldIdxSize) = case mrepo0 of
                                                [] -> (Nothing,sha256hash mempty, 0)
                                                [(repo_id,h,sz)] -> (Just repo_id, h, sz)
                                                (_:_:_) -> error "the impossible happened"

      ifn <- getIndexTarFn hackageRepoId
      withFile ifn ReadMode $ \h -> do
        hSetBinaryMode h True
        hSeek h SeekFromEnd 0
        fsz <- hTell h

        let (eofofs, 0) = quotRem (fromInteger fsz) 512
            deltaIdxSize = compare eofofs oldIdxSize
            oldMaxOfs = case oldIdxSize of
                       0 -> 0
                       1 -> error "the impossible happened"
                       _ -> oldIdxSize-2

        case deltaIdxSize of
          EQ -> logDebug "index unchanged since last sync"
          LT -> fail "Index shrunk since last sync! Try removing cache.db to recover"
          GT | noSync -> logInfo "Index grown since last sync but ignoring due to --no-sync"
          GT -> do
            -- index grown grown
            logInfo "idx size changed..."
            (ctx1, hash1) <- case oldIdxSize of
                               0 -> pure (SHA256.init, sha256hash mempty)
                               1 -> fail "the impossible happened"
                               _ -> do
                                 ctx1' <- hSha256Update h 0 oldMaxOfs SHA256.init
                                 pure (ctx1', (sha256finalize (SHA256.update ctx1' (BS.replicate 1024 0x00))))

            -- print (hash1, oldIdxHash)
            unless (hash1 == oldIdxHash) $ fail "Index changed in a non-incremental way! Try removing cache.db to recover"

            ctx2 <- hSha256Update h oldMaxOfs (eofofs-2) ctx1
            let hash2 = (sha256finalize (SHA256.update ctx2 (BS.replicate 1024 0x00)))

            -- print hash2

            DB.withTransaction conn $ do
              _repoId <- case mrepoId of
                Nothing -> do
                  DB.execute conn "INSERT INTO repos(repo_name,idxhash,blocks) VALUES(?,?,?)" (hackageRepoId, hash2, eofofs)
                  [Only repo_id] <- DB.query conn "SELECT repo_id FROM repos WHERE repo_name = ?" (Only hackageRepoId)
                  return repo_id
                Just repo_id -> do
                  DB.execute conn "UPDATE repos SET idxhash = ?, blocks = ? WHERE repo_id = ?" (hash2, eofofs, repo_id :: Int)
                  return repo_id

              -- TODO: make use of _repoId as namespace id
              newMaxOfs <- readTarEntries'' h oldMaxOfs go

              unless (2+newMaxOfs == eofofs) $ fail "last-entry offset sanity check failed"

            DB.execute_ conn "VACUUM"

        runHIX act (conn,h)

getDbVer :: Path Absolute -> IO (Maybe Int)
getDbVer dbfn = do
  ex <- doesFileExist dbfn
  if ex
    then DB.withConnection (toFilePath dbfn) $ \conn -> do
           [Only dbver] <- DB.query_ conn "PRAGMA user_version"
           return (Just dbver)
    else return Nothing

enableFK :: DB.Connection -> IO ()
enableFK conn = do
  DB.execute_ conn "PRAGMA foreign_keys = ON"

resetDb :: Path Absolute -> IO ()
resetDb dbfn = do
  exists <- doesFileExist dbfn
  when exists $ removeFile dbfn
  DB.withConnection (toFilePath dbfn) $ \conn -> DB.withExclusiveTransaction conn $ do
    [Only dbver] <- DB.query_ conn "PRAGMA user_version"
    enableFK conn
    unless (dbver == (0 :: Int)) $ fail "resetDb: the impossible happened"
    DB.execute_ conn (DB.Query ("PRAGMA user_version = " <> tshow curDbVer))

    DB.execute_ conn "CREATE TABLE pnames (pname_id INTEGER PRIMARY KEY, pname TEXT UNIQUE NOT NULL)"
    DB.execute_ conn "CREATE TABLE vers   (ver_id   INTEGER PRIMARY KEY, ver   TEXT UNIQUE NOT NULL)"
    DB.execute_ conn "CREATE TABLE mnames (mname_id INTEGER PRIMARY KEY, mname TEXT UNIQUE NOT NULL)"
    DB.execute_ conn "CREATE TABLE tools  (tool_id INTEGER PRIMARY KEY, tool TEXT UNIQUE NOT NULL)"
    DB.execute_ conn "CREATE TABLE syns   (syn_id INTEGER PRIMARY KEY, syn TEXT UNIQUE NOT NULL)"

    DB.execute_ conn "CREATE TABLE repos (\
                     \  repo_id   INTEGER PRIMARY KEY, \
                     \  repo_name TEXT UNIQUE NOT NULL, \
                     \  blocks    INTEGER NOT NULL, \
                     \  idxhash   BLOB NOT NULL \
                     \)"

    -- TODO below this point: extend where needed tables with 'repo_id' to serve as namespace id
    DB.execute_ conn "CREATE TABLE unames (\
                     \  uname_id  INTEGER PRIMARY KEY, \
                     \  uname     TEXT UNIQUE NOT NULL \
                     \)"

    DB.execute_ conn "CREATE TABLE pkgids (\
                     \  pkgid    INTEGER PRIMARY KEY, \
                     \  pname_id INTEGER NOT NULL, \
                     \  ver_id   INTEGER NOT NULL, \
                     \  revcnt   INTEGER NOT NULL DEFAULT 0, \

                     \  FOREIGN KEY(pname_id) REFERENCES pnames(pname_id), \
                     \  FOREIGN KEY(ver_id) REFERENCES vers(ver_id), \
                     \  UNIQUE(ver_id, pname_id) \
                     \)"

    -- the 3 relations below (revisions, prefs, tars)
    DB.execute_ conn "CREATE TABLE revisions (\
                     \  pkgid    INTEGER NOT NULL, \
                     \  rev      INTEGER NOT NULL, \
                     \  srev     INTEGER NOT NULL, \
                     \  ts       INTEGER NOT NULL, \
                     \  ofs      INTEGER NOT NULL, \
                     \  uname_id INTEGER NOT NULL, \
                     \  syn_id   INTEGER NOT NULL, \
                     \  cabsha   BLOB NOT NULL, \

                     \  FOREIGN KEY(pkgid) REFERENCES pkgids(pkgid), \
                     \  FOREIGN KEY(uname_id) REFERENCES unames(uname_id), \
                     \  FOREIGN KEY(syn_id) REFERENCES syns(syn_id), \
                     \  PRIMARY KEY(pkgid,rev) \
                     \)"

    DB.execute_ conn "CREATE TABLE prefs (\
                     \  pname_id INTEGER NOT NULL, \
                     \  ts       INTEGER NOT NULL, \
                     \  ofs      INTEGER NOT NULL, \

                     \  FOREIGN KEY(pname_id) REFERENCES pnames(pname_id), \
                     \  PRIMARY KEY(pname_id,ts) \
                     \)"

    -- TODO: This is only prepared for tar-mutation
    --
    -- 'ts' is NOT always the upload time
    --
    -- invariant for association with 'revisions':
    -- 1.) each tar-mutation is associated to at least one revision, i.e. 1:N
    -- 2.) for correlated entries, tar.ts >= revision.ts > prev(tar-for-pkgid).ts holds
    DB.execute_ conn "CREATE TABLE tars (\
                     \  pkgid    INTEGER NOT NULL, \
                     \  srev     INTEGER NOT NULL, \
                     \  ts       INTEGER NOT NULL, \
                     \  ofs      INTEGER NOT NULL, \
                     \  tarsha   BLOB NOT NULL, \
                     \  tarsz    INTEGER NOT NULL, \

                     \  FOREIGN KEY(pkgid) REFERENCES pkgids(pkgid), \
                     \  PRIMARY KEY(pkgid,srev) \
                     \)"

    DB.execute_ conn "CREATE TABLE pkgids_mnames (\
                     \  pkgid    INTEGER NOT NULL, \
                     \  mname_id INTEGER NOT NULL, \

                     \  FOREIGN KEY(pkgid) REFERENCES pkgids(pkgid), \
                     \  FOREIGN KEY(mname_id) REFERENCES mnames(mname_id), \
                     \  PRIMARY KEY(pkgid,mname_id) \
                     \)"

    DB.execute_ conn "CREATE TABLE pkgids_tools (\
                     \  pkgid    INTEGER NOT NULL, \
                     \  tool_id INTEGER NOT NULL, \

                     \  FOREIGN KEY(pkgid) REFERENCES pkgids(pkgid), \
                     \  FOREIGN KEY(tool_id) REFERENCES tools(tool_id), \
                     \  PRIMARY KEY(pkgid,tool_id) \
                     \)"

    DB.execute_ conn "CREATE TABLE pkgids_deps (\
                     \  pkgid    INTEGER NOT NULL, \
                     \  pname_id INTEGER NOT NULL, \

                     \  FOREIGN KEY(pkgid) REFERENCES pkgids(pkgid), \
                     \  FOREIGN KEY(pname_id) REFERENCES pnames(pname_id), \
                     \  PRIMARY KEY(pkgid,pname_id) \
                     \)"


    -- VIEWS
    DB.execute_ conn "CREATE VIEW pkgids_str AS \
                     \SELECT pi.pkgid, n.pname, v.ver \
                     \FROM pkgids pi, pnames n, vers v \
                     \WHERE pi.pname_id = n.pname_id AND pi.ver_id = v.ver_id"

    -- NOTE: We could add an index
    --
    --   CREATE INDEX pkgids_mnames_index ON pkgids_mnames(mname_id)
    --
    -- but this would require significant storage overhead.

curDbVer :: Int
curDbVer = 25
