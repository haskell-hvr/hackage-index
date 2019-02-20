{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright: Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-3.0-or-later
--
module Cabal.Config where

import           Utils

import qualified Data.Map.Strict            as M
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Distribution.Parsec.Parser
import           System.Directory           (getAppUserDataDirectory)
import           System.Environment         (lookupEnv)
import           System.Path.IO

hackageRepoId :: T.Text
hackageRepoId = "hackage.haskell.org"

-- | Locate cabal configuration
locateConfigFile :: IO (Path Absolute)
locateConfigFile = do
  fp <- fmap fromFilePath (maybe (getAppUserDataDirectory "cabal") return =<< lookupEnv "CABAL_DIR")
  dir <- makeAbsolute fp
  return $ dir </> fragment "config"


data CabalConfig = CabalConfig
    { ccRepos :: [(T.Text,(Bool,Path Absolute))] -- (label, (is-secure,fspath-to-index-tar))
    } deriving (Show)

loadConfigFile :: Path Absolute -> IO CabalConfig
loadConfigFile fp = do
    raw <- readStrictByteString fp

    let Right xs = readFields raw

    basedir' <- case [ val | Field (Name _ "remote-repo-cache") [FieldLine _ val] <- xs ] of
      []   -> fail ("remote-repo-cache not set in " ++ show (toFilePath fp))
      vals -> return (bs2fp (last vals))

    basedir <- makeAbsolute basedir'

    repos <- forM [ (label,props) | Section (Name _ "repository") [SecArgName _ label] props <- xs ] $ \(l,props') -> do
      let props = M.fromList [ (k,[ l' | FieldLine _ l' <- ls ]) | Field (Name _ k) ls <- props' ]
      let l' = T.decodeUtf8 l
          d = basedir </> fragment (T.unpack l')

      isSec <- case M.lookup "secure" props of
                 Nothing  -> pure $! l' == hackageRepoId -- magic default
                 Just [s] -> maybe (fail ("Failed to parse 'repository[" ++ show l' ++ "].secure' value " ++ show s))
                                   pure (toBool s)
                 Just ss -> fail ("Failed to parse 'repository[" ++ show l' ++ "].secure' value " ++ show ss)

      pure (l', (isSec,d))

    return (CabalConfig repos)
  where
    bs2fp = fromFilePath . T.unpack . T.decodeUtf8

    toBool s = case T.toLower (T.decodeUtf8 s) of
                 "false" -> Just False
                 "true"  -> Just True
                 _       -> Nothing
