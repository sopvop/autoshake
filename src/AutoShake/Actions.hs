{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module AutoShake.Actions
       where


import           Control.Lens                     (use, uses, view, (%=), (.=),
                                                   (^.))
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Trans.Except       (ExceptT (..), runExceptT,
                                                   throwE)
import           Control.Monad.Trans.State.Strict (StateT, execStateT,
                                                   runStateT)
import qualified Data.ByteString                  as B
import           Data.Foldable                    (foldlM)
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as HashMap
import           Data.List                        ((\\))
import           Data.Maybe                       (fromMaybe)

import           Data.Monoid                      ((<>))
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Text.PrettyPrint.ANSI.Leijen     (Doc, Pretty (..), pretty)
import qualified Text.PrettyPrint.ANSI.Leijen     as PP
import           Text.Trifecta.Delta              (Delta, HasDelta (..), bytes,
                                                   rewind)
import           Text.Trifecta.Rendering          (renderingCaret)

import           AutoShake.Ast                    (BoolTerm (..), Command (..),
                                                   Expr (..), HasCommand (..),
                                                   Identifier (..),
                                                   IntTerm (..), Scope (..),
                                                   ScopeAct (..), ScopeOp (..),
                                                   StringTerm (..), Term (..),
                                                   TopItem (..), boolTermVal,
                                                   identifierText, intTermVal,
                                                   stringTermVal)
import           AutoShake.Core
import           AutoShake.Parser                 (parseFile)
import           System.IO                        (stderr)

class TypeChecked a where
  checked :: Value Delta -> Either Error a

instance TypeChecked Bool where
  checked (VBool b _) = Right b
  checked v = Left (typeError "bool" v)

instance TypeChecked Int where
  checked (VInt b _) = Right b
  checked v = Left (typeError "integer" v)

instance TypeChecked Text where
  checked (VString b _) = Right b
  checked v = Left (typeError "string" v)

instance TypeChecked [Text] where
  checked (VStringList r _) = Right r
  checked v = Left (typeError "strings list" v)

instance TypeChecked [Reference] where
  checked (VStringList r _) = Right (map (GlobalReference "") r) --Check for real
  checked v = Left (typeError "label expression" v)

typeError str v = case v of
    VBool _ d -> got d "bool"
    VInt _ d -> got d "integer"
    VString _ d -> got d "string"
    VStringList _ d -> got d "strings list"
    VMap _ d -> got d "scope"
  where
     got d g = Error d (PP.string $ "Expected " <> str <> " got: " <> g)


newtype ExprChecker a = ExprChecker (
  (HashMap Text (Value Delta), String) -> Either Error a)
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

argOpt :: TypeChecked a => Text -> ExprChecker (Maybe a)
argOpt nm = ExprChecker $ \(hm, name) ->
   case HashMap.lookup nm hm of
      Nothing -> Right Nothing
      Just v -> case checked v of
          Right r -> Right (Just r)
          Left e -> Left e --["Error parsing config for " <> name <> ": " <> e]

argDef :: TypeChecked a => Text -> a -> ExprChecker a
argDef nm def = do
  t <- argOpt nm
  case t of
    Nothing -> pure def
    Just v -> pure v

arg :: TypeChecked a => Text -> ExprChecker a
arg nm = do
   t <- argOpt nm
   case t of
       Nothing -> ExprChecker $ \(_, name) ->
         Left $ Error mempty (PP.string (name <> " requires " <> T.unpack nm <> " config option"))
       Just v -> pure v


runExprChecker :: String
               -> HashMap Text (Value Delta)
               -> ExprChecker t
               -> Either Error t
runExprChecker tool hm (ExprChecker f) = f (hm, tool)


type ModuleChecker a = ExceptT Error (StateT Env IO) a


fileCheck :: MonadIO m => String -> ExceptT Error m [TopItem Delta]
fileCheck s = do
  r <- parseFile s
  case r of
    Nothing -> throwError mempty ("Error parsing file: " <> s)
    Just ast -> pure ast

expectStringList :: Monad m => Value Delta -> ExceptT Error m (Delta, [Text])
expectStringList v = case v of
  VStringList ss d -> pure (d, ss)
  _ -> throwError (v^.valueAnn)  ("Expected string list")

lookupStringList d nm scope =
  case HashMap.lookup nm scope of
    Nothing -> pure (d, [])
    Just v -> case v of
      (VStringList ss del) -> pure (del, ss)
      _ -> throwError d ("Key is not strings list")

mergeLists :: ([Text] -> [Text] -> [Text])
           -> Identifier Delta
           -> HashMap Text ValueAt
           -> Value Delta
           -> ModuleChecker (HashMap Text ValueAt)
mergeLists f (Identifier nm d) scope val =
   go <$> expectStringList val
      <*> lookupStringList d nm scope
   where
     go (d, xs) (_, ys) = HashMap.insert nm (VStringList (f xs ys) d) scope

toValue :: Applicative f => Term a -> f (Value a)
toValue v = case v of
  TBool (BoolTerm b a) -> pure $ VBool b a
  TInt (IntTerm i a) -> pure $ VInt i a
  TString (StringTerm i a) -> pure $ VString i a
  TStringList vs a -> pure $ VStringList (map (^.stringTermVal) vs) a

runExpr _ (ETerm v) = toValue v
runExpr _ v = throwError (delta v) ("Functions not yet supported")

runScope :: Scope Delta -> ModuleChecker (HashMap Text ValueAt)
runScope (Scope acts d) = foldlM go mempty acts
  where
    go scope (ScopeAct nm op expr) = do
      val <- runExpr scope expr
      case op of
        ScopeAssign -> pure $ HashMap.insert (nm^.identifierText) val scope
        ScopeAppend -> mergeLists (++) nm scope val
        ScopeDiff -> mergeLists (\\) nm scope val

handleConfig :: Command Delta -> FilePath -> ModuleChecker ()
handleConfig t _ = do
  let (Identifier label d) = t^.commandLabel
      --name = Name (T.pack path) label
  dict <- runScope (t^.commandConfig)
  let config = Config d dict
  let r = runExprChecker "config" dict $
           TargetConfig d <$> argDef "depends" []
                          <*> argDef "configs" []
                          <*> argDef "public_configs" []
                          <*> pure config
  conf <- case r of
    Left e -> throwE e
    Right r -> pure r
  envThisFile.moduleConfigs %= HashMap.insert label conf

moduleCheck  ::  [TopItem Delta] -> ModuleChecker File
moduleCheck targets = do
    mapM_ checkItem targets
    use envThisFile
  where
    checkItem (TopInclude s _) = do
      loaded <- uses envFiles (HashMap.lookup s)
      case loaded of
        Nothing -> do
           res <- fileCheck s
           r <- moduleCheck res
           envFiles %= HashMap.insert s r
           envImports %= HashMap.insert s r
        Just v -> do
          envImports %= HashMap.insert s v
    checkItem (TopTarget t) = do
      case t^.commandName.identifierText of
        "template" -> undefined
        "config" -> use envFilePath >>= handleConfig t
        _ -> undefined

runFileCheck :: FilePath -> String -> IO (Either Error (Env, File))
runFileCheck root fileName = do
  r <- parseFile fileName
  case r of
    Nothing -> return $ Left (mkError mempty "Error parsing file")
    Just ast -> do
      (a, st) <- runStateT (runExceptT (moduleCheck ast)) (emptyEnv root fileName)
      case a of
        Left e -> return $ Left e
        Right r -> return $ Right (st, r)

execFileCheck root fileName = do
  r <- runFileCheck root fileName
  case r of
    Left e -> renderError fileName e >>= PP.hPutDoc stderr
    Right r -> print r
{-
groupTool nm = go <$> arg "sources"
  where
    go = Target nm
-}

