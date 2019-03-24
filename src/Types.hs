{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

-- |
-- Copyright: Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-3.0-or-later
--
module Types
    ( PkgV(..), groupCaret, vstrip0s

    , PkgN, mkPkgN
    , PkgId(..)
    , PkgIdR(..), pkgIdRParser
    , ToolN(..)
    , UserN(..)
    , ModuleN, mkModuleN
    , PkgR(..)
    , PkgNOrId(..)

    , SHA256Val, sha256hash, sha256zero, sha256finalize, hSha256Update, sha256unhex
    , MD5Val, md5zero, md5unhex

    ) where

import           Utils

import qualified Codec.Archive.Tar.Index          as Tar
import qualified Codec.Base16                     as B16
import qualified Crypto.Hash.SHA256               as SHA256
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as BS
import           Data.ByteString.Short            (ShortByteString, fromShort,
                                                   toShort)
import qualified Data.ByteString.Short            as SBS
import           Data.Char                        (isDigit)
import qualified Data.List                        as List
import qualified Data.Set                         as Set
import           Data.String
import qualified Data.Text                        as T
import qualified Database.SQLite.Simple.FromField as DB
import qualified Database.SQLite.Simple.FromRow   as DB
import qualified Database.SQLite.Simple.ToField   as DB
import           Distribution.ModuleName          as C
import           Distribution.Package             as C
import qualified Distribution.Text                as C
import qualified Distribution.Version             as C
import qualified Options.Applicative              as OA
import           System.Path.IO
import           Text.Parsec                      as P

-- Various Text-ish types
newtype ToolN   = ToolN   T.Text deriving (Eq,Ord,NFData,DB.ToField,DB.FromField)
newtype ModuleN = ModuleN T.Text deriving (Eq,Ord,NFData,DB.ToField,DB.FromField)
newtype UserN   = UserN   T.Text deriving (Eq,Ord,NFData,DB.ToField,DB.FromField,Show)

-- package-id types
newtype PkgN    = PkgN    T.Text deriving (Eq,Ord,NFData,DB.ToField,DB.FromField,Show)
newtype PkgR    = PkgR    Int    deriving (Eq,NFData,DB.ToField,DB.FromField)

data PkgId  = PkgId  !PkgN !PkgV
data PkgIdR = PkgIdR !PkgN !PkgV !PkgR

data PkgNOrId = PkgId' !PkgId
              | PkgN'  !PkgN

mkModuleN :: C.ModuleName -> ModuleN
mkModuleN = ModuleN . tdisp'

mkPkgN :: C.PackageName -> PkgN
mkPkgN = PkgN . tdisp'

instance TPretty PkgId where
  tdisp (PkgId n v) = mconcat [ tdisp n, "-", tdisp v ]
  disp  (PkgId n v) = mconcat [ disp n, "-", disp v ]

instance DB.FromRow PkgId where
  fromRow = uncurry PkgId <$> DB.fromRow

-- | @foo-1.2.3-r2@
instance TPretty PkgIdR where
  tdisp (PkgIdR n v r) = mconcat [ tdisp n, "-", tdisp v, "-", tdisp r ]
  disp  (PkgIdR n v r) = mconcat [ disp n, "-", disp v, "-", disp r ]

instance TPretty PkgR where
  disp (PkgR i) = 'r':show i

----------------------------------------------------------------------------

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

instance IsString SHA256Val where
    fromString = fromMaybe (error "invalid SHA256Val string-literal") . sha256unhex . fromString

instance Show SHA256Val where
    show = ("sha256:" ++) . show . sha256hex

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

----------------------------------------------------------------------------

newtype MD5Val    = MD5Val    ShortByteString
                  deriving (Eq,Ord,NFData)

instance IsString MD5Val where
    fromString = fromMaybe (error "invalid MD5Val string-literal") . md5unhex . fromString

instance Show MD5Val where
    show = ("md5:" ++) . show . md5hex

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

----------------------------------------------------------------------------

instance TPretty PkgN    where tdisp (PkgN t) = t
instance TPretty ToolN   where tdisp (ToolN t) = t
instance TPretty ModuleN where tdisp (ModuleN t) = t
instance TPretty UserN   where tdisp (UserN t) = t

instance Reader PkgN where
  readm = parsecReader "PKG-NAME" pkgNameParser

instance Reader ModuleN where
  readm = parsecReader "MODULE-NAME" modNameParser

instance Reader ToolN where
  readm = OA.eitherReader (\s -> pure (ToolN . T.pack $ s)) -- FIXME

instance Reader PkgIdR where
  readm = parsecReader "PKGID-REV" pkgIdRParser

instance Reader PkgNOrId where
  readm = parsecReader "PKG-NAME|PKGID" pkgNameOrIdParser

pkgNameOrIdParser :: Parsec T.Text () PkgNOrId
pkgNameOrIdParser = (PkgN' <$> P.try pkgNameParser) <|> (PkgId' <$> pkgIdParser)

pkgNameParser :: Parsec T.Text () PkgN
pkgNameParser = PkgN . T.pack <$> do
    List.intercalate "-" <$> P.sepBy1 component (P.char '-')
  where
    component = do
      cs <- P.many1 alphaNum
      when (all isDigit cs) (fail "packag-name digits-only name component")
      return cs

pkgIdParser :: Parsec T.Text () PkgId
pkgIdParser = do
    pn <- List.intercalate "-" <$> P.endBy1 ncomponent (P.char '-')
    pv <- pkgVerParser
    pure (PkgId (PkgN (T.pack pn)) pv)
  where
    ncomponent = P.try $ do
      cs <- P.many1 alphaNum
      when (all isDigit cs) (fail "packag-name digits-only name component")
      return cs

pkgIdRParser :: Parsec T.Text () PkgIdR
pkgIdRParser = do
  PkgId pn pv <- pkgIdParser
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

----------------------------------------------------------------------------

newtype PkgV = PkgV C.Version deriving (Eq,Ord,NFData,C.Text)

instance TPretty PkgV where
  disp = display

instance DB.ToField PkgV where
  toField = DB.toField . tdisp

instance DB.FromField PkgV where
  fromField fld = do
    x <- DB.fromField fld
    case tparse x of
      Nothing -> fail "failed to convert to PkgV"
      Just v  -> pure $! PkgV v

instance Reader PkgV where
  readm = parsecReader "VERSION" pkgVerParser

pkgVerParser :: Parsec T.Text () PkgV
pkgVerParser = do
    pv <- map read <$> P.sepBy1 vcomponent (P.char '.')
    pure (PkgV (C.mkVersion pv))
  where
    vcomponent = do
      n <- P.many1 P.digit
      case n of
        ('0':_:_) -> fail "redundant leading 0 in version component"
        _         -> pure n

-- | Normalise by stripping 0 suffixes from version
vstrip0s :: PkgV -> PkgV
vstrip0s = coerce vstrip'
  where
    vstrip' = C.alterVersion $ \case
      [0] -> [0]
      ys  -> List.dropWhileEnd (==0) ys

-- | Groups a \"characteristic\" set of versions as disjunction of @^>=@ constraints
groupCaret :: Set.Set (PkgV,Bool) -> [(PkgV,Maybe PkgV)]
groupCaret = go . groupOn (vmaj . fst) . Set.toList
  where
    go :: [([(PkgV,Bool)],PkgV)] -> [(PkgV,Maybe PkgV)]
    go []              = []
    go ((vs,_mj):rest) = go1 (groupOnSnd vs) ++ go rest

    go1 :: [([PkgV],Bool)] -> [(PkgV,Maybe PkgV)]
    go1 []                           = []
    go1 ((_,False):rest)             = go1 rest
    go1 ((vs,True):(vs',False):rest) = (head vs, Just (head vs')) : go1 rest
    go1 [(vs,True)]                  = [(head vs,Nothing)]
    go1 ((_,True):(_,True):_)        = error "the impossible happened"

    vmaj :: PkgV -> PkgV
    vmaj = coerce vmaj'
      where
        vmaj' = C.alterVersion $ \case
          []      -> undefined
          [x]     -> [x,0]
          (x:y:_) -> [x,y]
