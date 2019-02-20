{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Copyright: Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-3.0-or-later
--
module PkgIdxTs where

import qualified Data.Text                        as T
import           Data.Time.Clock.POSIX            (POSIXTime,
                                                   posixSecondsToUTCTime,
                                                   utcTimeToPOSIXSeconds)
import           Data.Time.Format                 (defaultTimeLocale,
                                                   formatTime, parseTimeM)
import           Data.Vector.Unboxed.Deriving     (derivingUnbox)
import qualified Database.SQLite.Simple.FromField as DB
import qualified Database.SQLite.Simple.ToField   as DB
import           Text.Parsec                      as P

import           Utils

newtype PkgIdxTs = PkgIdxTs Int
                 deriving (Eq,Ord,Show,DB.FromField,DB.ToField)

derivingUnbox "PkgIdxTs" [t| PkgIdxTs -> Int |] [| \(PkgIdxTs i) -> i |] [| PkgIdxTs |]

fmtPkgIdxTs :: PkgIdxTs -> String
fmtPkgIdxTs (PkgIdxTs t) = formatTime defaultTimeLocale "%Y-%m-%dT%TZ" (posixSecondsToUTCTime (fromIntegral t :: POSIXTime))

pkgIdxTsParser :: Parsec T.Text () PkgIdxTs
pkgIdxTsParser = try $ do
  -- 2006-11-02T14:22:57Z

  str <- sequence
    [ digit, digit, digit, digit
    , char '-'
    , digit, digit
    , char '-'
    , digit, digit
    , char 'T' <|> char 't'
    , digit, digit
    , char ':'
    , digit, digit
    , char ':'
    , digit, digit
    , char 'Z' <|> char 'z'
    ]

  case parseTimeM True defaultTimeLocale "%Y-%m-%dT%TZ" (str :: [Char]) of
    Nothing -> fail "invalid ISO8601 format"
    Just t  -> return $ PkgIdxTs $ ceiling (realToFrac (utcTimeToPOSIXSeconds t) :: Double)


data TsRef = TsRef0
           | TsRefTs !PkgIdxTs
           | TsRefLatest
           | TsRefPkgId !PkgIdR

tsRefParser :: Parsec T.Text () TsRef
tsRefParser = do
  choice [ char '@' *> pure TsRefLatest
         , TsRefTs <$> pkgIdxTsParser
         , TsRefPkgId <$> pkgIdRParser
         ]

data TsRange = TsRange !TsRef !TsRef

tsRangeParser :: Parsec T.Text () TsRange
tsRangeParser = do
  --    tsref [ ".." [ tsref ] ]

  r1 <- tsRefParser

  mr2 <- optionMaybe $ do
    _ <- string ".."
    r2 <- optionMaybe tsRefParser
    let r2' = maybe TsRefLatest id r2
    pure r2'

  pure $ case mr2 of
    Nothing -> TsRange TsRef0 r1
    Just r2 -> TsRange r1 r2


instance Reader TsRef where
  readm = parsecReader "TSREF" tsRefParser

instance Reader TsRange where
  readm = parsecReader "TSRANGE" tsRangeParser


