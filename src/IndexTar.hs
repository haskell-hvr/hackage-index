{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Copyright:  Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-3.0-or-later
--
module IndexTar where

import           Utils

import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Index as Tar
import qualified Codec.Compression.GZip  as GZip
import qualified Data.Aeson              as J
import qualified Data.Aeson.Types        as J
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.ByteString.Short   as BSS
import qualified Data.HashMap.Strict     as HM
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           System.Path.IO

import           Cabal.Config

type SrcTarName   = BSS.ShortByteString -- with .tar.gz suffix

getIndexTarFn :: T.Text -> IO (Path Absolute)
getIndexTarFn label = do
    CabalConfig repos <- loadConfigFile =<< locateConfigFile

    when (null repos) $ fail "no package indices found in cabal config"

    indexTarFp <- case (filter ((== label) . fst) repos) of
      []            -> fail ("repo with label " ++ show label ++ " not found")
      (_:_:_)       -> fail ("multiple repos found for label " ++ show label)
      [(_,(False,_))] -> fail ("repo with label " ++ show label ++ " is not a hackage-security repo")
      [(_,(True,x))]  -> return x

    return (indexTarFp </> fragment "01-index.tar")

readTarEntries'' :: Handle
                 -> Tar.TarEntryOffset
                 -> (Tar.Entry -> Tar.TarEntryOffset -> Tar.TarEntryOffset -> Maybe BS.ByteString -> IO ())
                 -> IO Tar.TarEntryOffset
readTarEntries'' h' ofs0 cb = go h' ofs0
  where
    go h ofs = do
      me <- Tar.hReadEntryHeaderOrEof h ofs
      case me of
        Nothing -> return ofs -- EOF
        Just (e, ofs') -> do
          case Tar.entryContent e of
            Tar.NormalFile _ sz -> do
              Tar.hSeekEntryContentOffset h ofs
              dat <- BS.hGet h (fromIntegral sz)
              cb e ofs ofs' (Just dat)
            _ -> cb e ofs ofs' Nothing

          go h ofs'

readTarEntries' :: T.Text -> IO [Tar.Entry]
readTarEntries' label = readTarEntries =<< getIndexTarFn label

readTarNormalFile1 :: Handle -> Tar.TarEntryOffset -> IO BS.ByteString
readTarNormalFile1 h ofs = do
      me <- Tar.hReadEntryHeaderOrEof h ofs
      case me of
        Nothing -> fail "readTarNormalFile1: EOF"
        Just (e, _) -> do
          case Tar.entryContent e of
            Tar.NormalFile _ sz -> do
              Tar.hSeekEntryContentOffset h ofs
              BS.hGet h (fromIntegral sz)
            _ -> fail "readTarNormalFile1: not a NormalFile"


-- | Convert to non-flat layout (i.e. @<name>/<ver>/<name>-<ver>.tar.gz@)
unFlat :: SrcTarName -> SrcTarName
unFlat fn0 = BSS.toShort $ mconcat [pn <> "/" <> pv <> "/" <> fn0']
  where
    fn0' = BSS.fromShort fn0

    Just base = stripSuffixBS ".tar.gz" fn0'

    (pn_, pv) = BS.spanEnd (\c -> (c >= 0x30 && c <= 0x3a) || c == 0x2e) base
    Just (pn, 0x2d) = BS.unsnoc pn_


-- | convert a filename  @<name>-<ver>.tar.gz@ into a @(<name>,<ver>)@ pkg-id pair
fn2pkgid :: SrcTarName -> (PkgN,PkgV)
fn2pkgid fn0 = (PkgN (T.decodeUtf8 pn), PkgV (T.decodeUtf8 pv))
  where
    fn0' = BSS.fromShort fn0

    Just base = stripSuffixBS ".tar.gz" fn0'

    (pn_, pv) = BS.spanEnd (\c -> (c >= 0x30 && c <= 0x3a) || c == 0x2e) base
    Just (pn, 0x2d) = BS.unsnoc pn_


-- | Read tarball lazily (and possibly decompress)
readTarEntries :: Path Absolute -> IO [Tar.Entry]
readTarEntries idxtar = do
    es <- case takeExtension idxtar of
            Just (FileExt "gz")  -> Tar.read . GZip.decompress <$> readLazyByteString idxtar
            Just (FileExt "tar") -> Tar.read                   <$> readLazyByteString idxtar
            ext         -> fail ("unknown extension " ++ show ext)

    return (Tar.foldEntries (:) [] (\err -> error ("readTarEntries " ++ show err)) es)

data IndexShaEntry = IndexShaEntry !SrcTarName !SHA256Val !MD5Val !Int
   deriving (Show)

-- | Decode and extract source-tarball filename and sha256 checksum from TUF @package.json@
decodePkgJsonFile :: BSL.ByteString -> Maybe IndexShaEntry
decodePkgJsonFile bs = do
    metainfo <- J.decode' bs
    [(fn,s256,m5,sz)] <- packagejson2sha metainfo

    s256' <- maybe (fail "bad SHA256 hash") pure $ sha256unhex s256
    m5'   <- maybe (fail "bad MD5 hash") pure $ md5unhex m5

    return $! IndexShaEntry (BSS.toShort $ normaliseFn fn) s256' m5' sz
  where
    normaliseFn fn = fromMaybe fn $ stripPrefixBS "<repo>/package/" fn

    packagejson2sha :: J.Value -> Maybe [(BS.ByteString, BS.ByteString, BS.ByteString, Int)]
    packagejson2sha = J.parseMaybe go1
      where
        go1 = J.withObject "PackageJson" $ \o -> do
            signed   <- o      J..: "signed"
            targets  <- signed J..: "targets"
            J.withObject "PackageJson.signed.targets" go2 targets

        go2 m = forM (HM.toList m) $ \(k,v) -> do
            J.withObject ".targets{}" (go3 k) v

        go3 k o = do
            hashes <- o      J..: "hashes"
            sh256  <- hashes J..: "sha256"
            m5     <- hashes J..: "md5"
            sz <- o J..: "length"
            return (T.encodeUtf8 k, T.encodeUtf8 sh256, T.encodeUtf8 m5, sz)
