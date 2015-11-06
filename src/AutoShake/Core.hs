{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
module AutoShake.Core
       where

import           Control.Lens.TH     (makeClassy)
import           Data.HashMap.Strict (HashMap)
import           Data.Text           (Text)

import           Text.Trifecta.Delta (Delta)

import           AutoShake.Ast       (Term (..))

data Value a = VBool !Bool a
             | VInt !Int a
             | VString !String a
             | VStringList ![String] a
             deriving (Eq, Show, Functor, Traversable, Foldable)

type ValueAt = Value Delta

data Config = Config !Delta (HashMap Text ValueAt)
            deriving (Eq, Show)

data Reference = LocalReference Text
               | GlobalRefence FilePath Text
               deriving (Eq, Show)

data Target = Target
  { _tgName     :: String
  , _tgProduces :: [FilePath]
  , _tgDepends  :: [FilePath]
  , _tgCommands :: [[String]]
  } deriving (Eq, Show)

data TargetConfig = TargetConfig
  { _tcLocation   :: Delta
  , _tcDepends    :: [Reference] -- Should be special path
  , _tcPublicConf :: [Reference] -- path to config
  , _tcConfig     :: Config      -- compiled config
  } deriving (Eq, Show)

data File = File
  { _moduleConfigs   :: HashMap Text Config
  , _moduleTemplates :: HashMap Text [TargetConfig]
  , _moduleTargets   :: HashMap Text [TargetConfig]
  } deriving (Show)

emptyFile :: File
emptyFile = File mempty mempty mempty

data Env = Env
 { _envFiles    :: HashMap FilePath File
 , _envThisFile :: File
 , _envRoot     :: FilePath
 , _envFilePath :: FilePath
 } deriving (Show)

emptyEnv :: FilePath -> FilePath -> Env
emptyEnv root mod = Env mempty emptyFile root mod

$(makeClassy ''File)
$(makeClassy ''Env)
