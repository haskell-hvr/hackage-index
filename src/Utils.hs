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
    ) where

import qualified Codec.Archive.Tar.Index          as Tar
import qualified Codec.Base16                     as B16
import           Control.DeepSeq
import           Control.Monad                    as X
import qualified Control.Monad.Reader             as X
import qualified Crypto.Hash.SHA256               as SHA256
import           Data.Bifunctor
import           Data.Bits
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as BS
import           Data.ByteString.Short            (ShortByteString, fromShort,
                                                   toShort)
import qualified Data.ByteString.Short            as SBS
import           Data.Char                        (isDigit)
import qualified Data.List                        as List
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       as X
import           Data.Semigroup
import qualified Data.Set                         as Set
import           Data.String
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as T
import           Data.Word                        as X
import           Database.SQLite.Simple           (Only (..))
import qualified Database.SQLite.Simple.FromField as DB
import qualified Database.SQLite.Simple.ToField   as DB
import qualified Distribution.Text                as C
import qualified Distribution.Version             as C
import           Numeric.Natural                  (Natural)
import qualified Options.Applicative              as OA
import qualified System.Directory                 as D
import           System.Exit                      (exitFailure)
import           System.IO                        (stderr)
import           System.Path.IO
import           Text.Parsec                      as P
import qualified Text.Parsec.Error                as P

-- Various Text-ish types
newtype ToolN   = ToolN   T.Text deriving (Eq,Ord,NFData,DB.ToField,DB.FromField)
newtype ModuleN = ModuleN T.Text deriving (Eq,Ord,NFData,DB.ToField,DB.FromField)
newtype UserN   = UserN   T.Text deriving (Eq,Ord,NFData,DB.ToField,DB.FromField,Show)

-- package-id types
newtype PkgN    = PkgN    T.Text deriving (Eq,Ord,NFData,DB.ToField,DB.FromField)
newtype PkgV    = PkgV    T.Text deriving (Eq,NFData,DB.ToField,DB.FromField)
newtype PkgR    = PkgR    Int    deriving (Eq,NFData,DB.ToField,DB.FromField)

data PkgId  = PkgId  !PkgN !PkgV
data PkgIdR = PkgIdR !PkgN !PkgV !PkgR

-- foo-1.2.3-r2
fmtPkgId :: PkgId -> String
fmtPkgId (PkgId (PkgN n) (PkgV v)) = mconcat [ T.unpack n, "-", T.unpack v ]

fmtPkgIdR :: PkgIdR -> String
fmtPkgIdR (PkgIdR (PkgN n) (PkgV v) (PkgR r)) = mconcat [ T.unpack n, "-", T.unpack v, "-r", show r ]




tshow :: Show a => a -> T.Text
tshow x = T.pack (show x)

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

newtype SHA256Val = SHA256Val ShortByteString
                  deriving (Eq,Ord,NFData)

instance DB.ToField SHA256Val where
  toField (SHA256Val x) = DB.toField (fromShort x)

instance DB.FromField SHA256Val where
  fromField fld = do
    x <- DB.fromField fld
    unless (BS.length x == 32) $ do
      fail "fromField: invalid SHA256Val in DB"
    return $! (SHA256Val (toShort x))

newtype MD5Val    = MD5Val    ShortByteString
                  deriving (Eq,Ord,NFData)

instance IsString SHA256Val where
    fromString = fromMaybe (error "invalid SHA256Val string-literal") . sha256unhex . fromString

instance IsString MD5Val where
    fromString = fromMaybe (error "invalid MD5Val string-literal") . md5unhex . fromString

instance Show SHA256Val where
    show = ("sha256:" ++) . show . sha256hex

instance Show MD5Val where
    show = ("md5:" ++) . show . md5hex

sha256hash :: ByteString -> SHA256Val
sha256hash = SHA256Val . toShort . SHA256.hash

sha256hex :: SHA256Val -> ByteString
sha256hex (SHA256Val x) = B16.encode (fromShort x)

sha256unhex :: ByteString -> Maybe SHA256Val
sha256unhex x = case B16.decode x of
    Right d | SBS.length d == 32
                -> Just (SHA256Val d)
    _           -> Nothing

-- Special reserved 'SHA256Val'
sha256zero :: SHA256Val
sha256zero = SHA256Val $ toShort $ BS.replicate 32 0

-- md5hash :: ByteString -> MD5Val
-- md5hash = MD5Val . toShort . MD5.hash

md5hex :: MD5Val -> ByteString
md5hex (MD5Val x) = B16.encode (fromShort x)

md5unhex :: ByteString -> Maybe MD5Val
md5unhex x = case B16.decode x of
    Right d | SBS.length d == 16
                -> Just (MD5Val d)
    _           -> Nothing

-- Special reserved 'SHA256Val'
md5zero :: MD5Val
md5zero = MD5Val $ toShort $ BS.replicate 16 0

sha256finalize :: SHA256.Ctx -> SHA256Val
sha256finalize = SHA256Val . toShort . SHA256.finalize

hSha256Update :: Handle -> Tar.TarEntryOffset -> Tar.TarEntryOffset -> SHA256.Ctx -> IO SHA256.Ctx
hSha256Update h ofs1 ofs2 ctx00 =
  case compare ofs1 ofs2 of
    GT -> fail "hSha256Update"
    EQ -> return ctx00
    LT -> do
      Tar.hSeekEntryOffset h ofs1
      go (ofs2 - ofs1) ctx00
  where
    go todo ctx0
      | todo > 0 = do
          let chunk = min 64 todo
          ctx' <- SHA256.update ctx0 <$> BS.hGet h (fromIntegral $ chunk * 512)
          go (todo-chunk) ctx'
      | otherwise = return ctx0


logDebug :: String -> IO ()
logDebug _ = pure ()

logInfo :: String -> IO ()
logInfo msg = T.hPutStrLn stderr (T.pack msg)

logWarn :: String -> IO ()
logWarn msg = T.hPutStrLn stderr ("WARNING: " <> T.pack msg)


nubSort :: Ord x => [x] -> [x]
nubSort = Set.toList . Set.fromList



newtype V = V C.Version
          deriving (C.Text,Ord,Eq,Show)


instance DB.ToField V where
  toField = DB.toField . tdisp

instance DB.FromField V where
  fromField fld = do
    x <- DB.fromField fld
    case tparse x of
      Nothing -> fail "failed to convert to V"
      Just v  -> pure $! v

tparse :: C.Text a => T.Text -> Maybe a
tparse = C.simpleParse . T.unpack

tdisp :: C.Text a => a -> T.Text
tdisp = T.pack . C.display

class Reader a where
  readm :: OA.ReadM a

instance Reader PkgN where
  readm = parsecReader "PKG-NAME" pkgNameParser

instance Reader ModuleN where
  readm = parsecReader "MODULE-NAME" modNameParser

instance Reader ToolN where
  readm = OA.eitherReader (\s -> pure (ToolN . T.pack $ s)) -- FIXME

instance Reader PkgIdR where
  readm = parsecReader "PKGID-REV" pkgIdRParser


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


pkgNameParser :: Parsec T.Text () PkgN
pkgNameParser = PkgN . T.pack <$> do
    List.intercalate "-" <$> P.sepBy1 component (P.char '-')
  where
    component = do
      cs <- P.many1 alphaNum
      when (all isDigit cs) (fail "packag-name digits-only name component")
      return cs

pkgIdParser :: Parsec T.Text () (PkgN,PkgV)
pkgIdParser = do
    pn <- List.intercalate "-" <$> P.endBy1 ncomponent (P.char '-')
    pv <- List.intercalate "." <$> P.sepBy1 vcomponent (P.char '.')
    pure (PkgN (T.pack pn), PkgV (T.pack pv))
  where
    ncomponent = P.try $ do
      cs <- P.many1 alphaNum
      when (all isDigit cs) (fail "packag-name digits-only name component")
      return cs

    vcomponent = do
      n <- P.many1 P.digit
      case n of
        ('0':_:_) -> fail "redundant leading 0 in version component"
        _         -> pure n

pkgIdRParser :: Parsec T.Text () PkgIdR
pkgIdRParser = do
  (pn,pv) <- pkgIdParser
  _ <- char '-'
  _ <- char 'r'
  n <- P.many1 P.digit
  case n of
    ('0':_:_) -> fail "redundant leading 0 in revision number"
    _         -> pure (PkgIdR pn pv (PkgR $ read n))

modNameParser :: Parsec T.Text () ModuleN
modNameParser = ModuleN . T.pack <$> s1
  where
    s1 = (:) <$> upper <*> s2
    s2 = choice
         [ (:) <$> (alphaNum <|> char '\'' <|> char '_') <*> s2
         , (:) <$> char '.' <*> s1
         , pure []
         ]

-- | Normalise by stripping 0 suffixes from version
vstrip0s :: V -> V
vstrip0s (V z) = V (vstrip' z)
  where
    vstrip' = C.alterVersion $ \case
      [0] -> [0]
      ys  -> List.dropWhileEnd (==0) ys


groupOn :: Ord k => (x->k) -> [x] -> [([x],k)]
groupOn g = map f . List.groupBy (\x y -> snd x == snd y) . map (\x -> (x, g x))
  where
    f xs = (map fst xs, snd (head xs))

groupOnSnd :: Eq b => [(v,b)] -> [([v],b)]
groupOnSnd =  map f . List.groupBy (\x y -> snd x == snd y)
  where
    f xs = (map fst xs, snd (head xs))

-- | Groups a \"characteristic\" set of versions as disjunction of @^>=@ constraints
groupCaret :: Set.Set (V,Bool) -> [(V,Maybe V)]
groupCaret = go . groupOn (vmaj . fst) . Set.toList
  where
    go :: [([(V,Bool)],V)] -> [(V,Maybe V)]
    go []              = []
    go ((vs,_mj):rest) = go1 (groupOnSnd vs) ++ go rest

    go1 :: [([V],Bool)] -> [(V,Maybe V)]
    go1 []                           = []
    go1 ((_,False):rest)             = go1 rest
    go1 ((vs,True):(vs',False):rest) = (head vs, Just (head vs')) : go1 rest
    go1 [(vs,True)]                  = [(head vs,Nothing)]
    go1 ((_,True):(_,True):_)        = error "the impossible happened"

    vmaj :: V -> V
    vmaj (V z) = V (vmaj' z)
      where
        vmaj' = C.alterVersion $ \case
          []      -> undefined
          [x]     -> [x,0]
          (x:y:_) -> [x,y]

-- | lift binary combining operation to 'Maybe's
onMaybes :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
onMaybes _ Nothing Nothing   = Nothing
onMaybes f (Just x) (Just y) = Just (f x y)
onMaybes _ (Just x) Nothing  = Just x
onMaybes _ Nothing (Just y)  = Just y

