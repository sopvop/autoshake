{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module AutoShake.Actions
       where

import           Control.Lens                     (use, uses, (%=), (.=))
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Trans.Except       (ExceptT (..), runExceptT,
                                                   throwE)
import           Control.Monad.Trans.State.Strict (StateT, execStateT)

import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as HashMap
import           Data.Monoid                      ((<>))
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Text.Trifecta.Delta              (Delta)

import           AutoShake.Ast                    (BoolTerm (..), IntTerm (..),
                                                   StringTerm (..), Term (..),
                                                   TopItem (..))
import           AutoShake.Core
import           AutoShake.Parser                 (parseFile)

class TypeChecked a where
  checked :: Term () Delta -> Either (Delta, String) a

instance TypeChecked Bool where
  checked (TBool (BoolTerm b _)) = Right b
  checked v = Left (typeError "bool" v)

instance TypeChecked Int where
  checked (TInt (IntTerm b _)) = Right b
  checked v = Left (typeError "integer" v)

instance TypeChecked Text where
  checked (TString (StringTerm b _)) = Right b
  checked v = Left (typeError "string" v)

instance TypeChecked [Text] where
  checked (TStringList r _) = Right $ fmap (\(StringTerm t _) -> t) r
  checked v = Left (typeError "strings list" v)

typeError str v = case v of
    TBool (BoolTerm _ d) -> (d, got "bool")
    TInt (IntTerm _ d) -> (d, got "integer")
    TString (StringTerm _ d) -> (d, got "string")
    TStringList _ d -> (d, "strings list")
  where
     got g = "Expected " <> str <> " got: " <> g


newtype ExprChecker a = ExprChecker (
  (HashMap Text (Term () Delta), String) -> Either [String] a)
                        deriving (Functor)

instance Applicative ExprChecker where
  pure a = ExprChecker $ \f -> Right a
  ExprChecker mf <*> ExprChecker ma = ExprChecker $ \args -> do
      f <- mf args
      a <- ma args
      pure (f a)

instance Monad ExprChecker where
  ExprChecker ma >>= f = ExprChecker $ \args -> do
    a <- ma args
    let (ExprChecker m) = f a
    m args

argOpt nm = ExprChecker $ \(hm, name) ->
   case HashMap.lookup nm hm of
      Nothing -> Right Nothing
      Just v -> case checked v of
          Right r -> Right (Just r)
          Left (d,e) -> Left ["Error parsing config for " <> name <> ": " <> e]

arg nm = do
   t <- argOpt nm
   case t of
       Nothing -> ExprChecker $ \(d, name) ->
         Left [name <> " requires " <> T.unpack nm <> " config option"]
       Just v -> pure v


runExprChecker :: String
               -> HashMap Text (Term () Delta)
               -> ExprChecker t
               -> Either [String] t
runExprChecker tool hm (ExprChecker f) = f (hm, tool)


type ModuleChecker a = ExceptT String (StateT Env IO) a

fileCheck s = do
  r <- parseFile s
  case r of
    Nothing -> throwE ("Error parsing file: " <> s)
    Just r -> pure r

moduleCheck  ::  [TopItem () Delta] -> ModuleChecker File
moduleCheck targets = do
    mapM_ checkItem targets
    use envThisFile
  where
    checkItem (TopInclude s d) = do
      loaded <- uses envFiles (HashMap.lookup s)
      case loaded of
        Nothing -> do
           res <- fileCheck s
           r <- moduleCheck res
           envFiles %= HashMap.insert s r
        Just v -> do
           return ()


runFileCheck root mod = do
  r <- parseFile mod
  case r of
    Nothing -> return $ Left ("Error parsing file: " <> mod)
    Just r -> Right <$> execStateT (runExceptT (moduleCheck r)) (emptyEnv root mod)

{-
groupTool nm = go <$> arg "sources"
  where
    go = Target nm
-}
