{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

-- |
-- Copyright:  Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-3.0-or-later
--
module HIX
    ( module HIX
    , TarEntryOffset
    )

where

import           Codec.Archive.Tar.Index (TarEntryOffset)
import           Control.Monad.Reader
import qualified Database.SQLite.Simple  as DB
import           IndexTar                (readTarNormalFile1)
import           System.Path.IO

import           Utils

-- | Monad representing a Hackage package index query context
newtype HIX a = HIX { unHIX :: ReaderT (DB.Connection,Handle) IO a }
              deriving (Functor,Applicative,Monad,MonadIO)

runHIX :: HIX a -> (DB.Connection,Handle) -> IO a
runHIX (HIX act) r = runReaderT act r

idxGetBlob :: TarEntryOffset -> HIX ByteString
idxGetBlob ofs = HIX $ do
  h <- asks snd
  liftIO (readTarNormalFile1 h ofs)

dbQuery_ :: DB.FromRow r => DB.Query -> HIX [r]
dbQuery_ dbq = HIX $ do
  conn <- asks fst
  liftIO (DB.query_ conn dbq)

dbQuery1_ :: DB.FromRow r => DB.Query -> HIX r
dbQuery1_ dbq = HIX $ do
  conn <- asks fst
  rows <- liftIO (DB.query_ conn dbq)
  case rows of
    [row] -> pure row
    [] -> liftIO (fail "dbQuery1_: internal programming error, query didn't return any rows")
    (_:_:_) -> liftIO (fail "dbQuery1_: internal programming error, query returned multiple rows")

dbQuery :: (DB.ToRow q, DB.FromRow r) => DB.Query -> q -> HIX [r]
dbQuery dbq args = HIX $ do
  conn <- asks fst
  liftIO (DB.query conn dbq args)

dbQuery1 :: (DB.ToRow q, DB.FromRow r) => DB.Query -> q -> HIX r
dbQuery1 dbq args = HIX $ do
  conn <- asks fst
  rows <- liftIO (DB.query conn dbq args)
  case rows of
    [row] -> pure row
    [] -> liftIO (fail "dbQuery1: internal programming error, query didn't return any rows")
    (_:_:_) -> liftIO (fail "dbQuery1: internal programming error, query returned multiple rows")

dbExecute :: DB.ToRow q => DB.Query -> q -> HIX ()
dbExecute dbq args = HIX $ do
  conn <- asks fst
  liftIO (DB.execute conn dbq args)

dbExecute_ :: DB.Query -> HIX ()
dbExecute_ dbq = HIX $ do
  conn <- asks fst
  liftIO (DB.execute_ conn dbq)

dbWithTx :: HIX a -> HIX a
dbWithTx (HIX act) = HIX $ do
    r@(conn,_) <- ask
    liftIO (DB.withTransaction conn (runReaderT act r))

dbWithExclusiveTx :: HIX a -> HIX a
dbWithExclusiveTx (HIX act) = HIX $ do
    r@(conn,_) <- ask
    liftIO (DB.withExclusiveTransaction conn (runReaderT act r))

unOnlyLst :: [Only x] -> [x]
unOnlyLst = coerce
