{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
module AutoShake.Core
       where

import           Control.Lens                 (Lens)
import           Control.Lens.TH              (makeClassy)
import           Control.Monad.Trans.Except   (ExceptT (..), runExceptT, throwE)
import qualified Data.ByteString              as B
import           Data.HashMap.Strict          (HashMap)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import           Text.PrettyPrint.ANSI.Leijen (Doc, Pretty (..), pretty)
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.Trifecta.Delta          (Delta, HasDelta (..), bytes,
                                               rewind)
import           Text.Trifecta.Rendering      (renderingCaret)


import           AutoShake.Ast                (Term (..))


data Error = Error Delta Doc

instance HasDelta Error where
  delta (Error d _) = d

instance Pretty (Error) where
  pretty (Error d doc) = pretty d <> PP.char ':' PP.<+> doc

mkError :: Delta -> String -> Error
mkError d s = Error d (PP.string s)

throwError :: Monad m => Delta -> String -> ExceptT Error m a
throwError d s = throwE (mkError d s)

grabLine bs = case B.elemIndex 10 bs of
  Nothing -> bs
  Just i -> B.take (i + 1) bs

renderError fname err = do
  body <- grabLine . B.drop (fromIntegral . bytes $ rewind d) <$> B.readFile fname
  return $ pretty err PP.<$> pretty (renderingCaret d body) <> PP.linebreak
  where
    d = delta err

instance Show Error where
  show (Error d doc) = show $ pretty d <> doc


data Value a = VBool !Bool a
             | VInt !Int a
             | VString !Text a
             | VStringList ![Text] a
             | VMap (HashMap Text ValueAt) a
             deriving (Eq, Show, Functor, Traversable, Foldable)

valueAnn :: Lens (Value a) (Value b) a b
valueAnn f v = case v of
  VBool b a -> VBool b <$> f a
  VInt i a -> VInt i <$> f a
  VString s a -> VString s <$> f a
  VStringList ss a -> VStringList ss <$> f a
  VMap m a -> VMap m <$> f a

{-# INLINE valueAnn #-}

type ValueAt = Value Delta

data Config = Config !Delta (HashMap Text ValueAt)
            deriving (Eq, Show)

data Reference = LocalReference Text
               | GlobalReference FilePath Text
               deriving (Eq, Show)

data Name d = Name { _namePath  :: Text
                   , _nameLabel :: Text
                   , _nameAnn   :: d
                   } deriving (Eq, Ord, Show)

data Target = Target
  { _tgName     :: String
  , _tgProduces :: [FilePath]
  , _tgDepends  :: [FilePath]
  , _tgCommands :: [[String]]
  } deriving (Eq, Show)

data TargetConfig = TargetConfig
  { _tcLocation   :: Delta
  , _tcDepends    :: [Reference] -- Should be special path
  , _tcConfigs    :: [Reference]
  , _tcPublicConf :: [Reference] -- path to config
  , _tcConfig     :: Config      -- compiled config
  } deriving (Eq, Show)

data File = File
  { _moduleConfigs   :: HashMap Text TargetConfig
  , _moduleTemplates :: HashMap Text [TargetConfig]
  , _moduleTargets   :: HashMap Text [TargetConfig]
  } deriving (Show)

emptyFile :: File
emptyFile = File mempty mempty mempty

data Env = Env
 { _envFiles    :: HashMap FilePath File
 , _envThisFile :: File
 , _envImports  :: HashMap FilePath File
 , _envRoot     :: FilePath
 , _envFilePath :: FilePath
 } deriving (Show)

emptyEnv :: FilePath -> FilePath -> Env
emptyEnv root mod = Env mempty emptyFile mempty root mod

$(makeClassy ''File)
$(makeClassy ''Env)
