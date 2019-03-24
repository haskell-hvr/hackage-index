{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

-- |
-- Copyright: Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-3.0-or-later
--
module Utils
    ( module Utils
    , module X
    , T.Text
    , C.display
    , ByteString
    , ShortByteString
    , Data.Semigroup.Semigroup((<>))
    , X.liftIO
    , toIntegralSized
    , Natural
    , exitFailure
    , Only(Only)
    , Map.Map
    , Set.Set
    , module Data.Coerce
    , for
    , List.partition
    ) where

import           Control.DeepSeq        as X
import           Control.Exception      as X (evaluate)
import           Control.Monad          as X
import qualified Control.Monad.Reader   as X
import           Data.Bifunctor         as X
import           Data.Bits
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import           Data.ByteString.Short  (ShortByteString)
import           Data.Coerce
import qualified Data.List              as List
import qualified Data.Map.Strict        as Map
import           Data.Maybe             as X
import           Data.Semigroup
import qualified Data.Set               as Set
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Data.Traversable
import           Data.Word              as X
import           Database.SQLite.Simple (Only (..))
import qualified Distribution.Text      as C
import           Numeric.Natural        (Natural)
import qualified Options.Applicative    as OA
import qualified System.Directory       as D
import           System.Exit            (exitFailure)
import           System.IO              (stderr)
import           System.Path.IO
import           Text.Parsec            as P
import qualified Text.Parsec.Error      as P

class Reader a where
  readm :: OA.ReadM a

-- | Convert 'Parsec' parser to 'OA.ReadM' parser
parsecReader :: String -> Parsec T.Text () a -> OA.ReadM a
parsecReader lbl p = OA.eitherReader (\s -> first (prerr s) . parse (p <* (eof <?> "end of string")) "" . T.pack $ s)
  where
    prerr :: String -> ParseError -> String
    prerr s0 e = List.intercalate "\n"
              [ "Error while parsing " ++ lbl ++ " argument:"
              , "'" ++ s0 ++ "'"
              , replicate (sourceColumn (errorPos e)) ' '
                ++ "^~~ " ++ (List.intercalate "; " (lines (showPError e)))
              ]

    showPError :: ParseError -> String
    showPError err = dropWhile (=='\n') $
                     P.showErrorMessages "OR" "unknown parse error" "expecting" "unexpected" "end of string" (P.errorMessages err)


----------------------------------------------------------------------------

-- | lift binary combining operation to 'Maybe's
onMaybes :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
onMaybes _ Nothing Nothing   = Nothing
onMaybes f (Just x) (Just y) = Just (f x y)
onMaybes _ (Just x) Nothing  = Just x
onMaybes _ Nothing (Just y)  = Just y

groupOn :: Ord k => (x->k) -> [x] -> [([x],k)]
groupOn g = map f . List.groupBy (\x y -> snd x == snd y) . map (\x -> (x, g x))
  where
    f xs = (map fst xs, snd (head xs))

groupOnSnd :: Eq b => [(v,b)] -> [([v],b)]
groupOnSnd =  map f . List.groupBy (\x y -> snd x == snd y)
  where
    f xs = (map fst xs, snd (head xs))

tshow :: Show a => a -> T.Text
tshow x = T.pack (show x)

tparse :: C.Text a => T.Text -> Maybe a
tparse = C.simpleParse . T.unpack

class TPretty a where
  {-# MINIMAL tdisp | disp #-}
  tdisp :: a -> T.Text
  tdisp = T.pack . disp

  disp :: a -> String
  disp = T.unpack . tdisp

tdisp' :: C.Text a => a -> T.Text
tdisp' = T.pack . C.display

getXdgCacheDirectory :: IO (Path Absolute)
getXdgCacheDirectory = makeAbsolute . fromFilePath =<< D.getXdgDirectory D.XdgCache ""

strictPair :: a -> b -> (a,b)
strictPair !a !b = (a,b)

stripPrefixBS :: ByteString -> ByteString -> Maybe ByteString
stripPrefixBS pfx b
  | BS.isPrefixOf pfx b = Just $ BS.drop (BS.length pfx) b
  | otherwise           = Nothing

stripSuffixBS :: ByteString -> ByteString -> Maybe ByteString
stripSuffixBS sfx b
  | BS.isSuffixOf sfx b = Just $ BS.take (BS.length b - BS.length sfx) b
  | otherwise           = Nothing

nubSort :: Ord x => [x] -> [x]
nubSort = Set.toList . Set.fromList

mapFromListDistinct :: Ord k => [(k, a)] -> Map.Map k a
mapFromListDistinct = Map.fromListWith (error "mapFromListDistinct")

----------------------------------------------------------------------------

logDebug :: String -> IO ()
logDebug _ = pure ()

logInfo :: String -> IO ()
logInfo msg = T.hPutStrLn stderr (T.pack msg)

logWarn :: String -> IO ()
logWarn msg = T.hPutStrLn stderr ("WARNING: " <> T.pack msg)
