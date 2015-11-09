{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
module AutoShake.Ast
       ( Identifier(..)
       , identifierText
       , Term(..)
       , StringTerm(..)
       , stringTermVal
       , BoolTerm(..)
       , boolTermVal
       , IntTerm(..)
       , intTermVal
       , TargetPath(..)
       , Command(..)
       , HasCommand(..)
       , Expr(..)
       , ScopeOp(..)
       , ScopeAct(..)
       , Scope(..)
       , TopItem(..)
       , Type(..)
       ) where

import           Control.Lens        (Lens, Lens')
import           Control.Lens.TH     (makeClassy)

import           Text.Trifecta.Delta (Delta, HasDelta (..))

import           Data.HashMap.Strict (HashMap)
import           Data.Text           (Text)
import           System.FilePath

data Identifier a = Identifier !Text a
                  deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

identifierText ::Lens' (Identifier a) Text
identifierText f (Identifier t a) = (\t' -> Identifier t' a) <$> f t

data StringTerm a = StringTerm !Text a
                  deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

stringTermVal :: Lens' (StringTerm a) Text
stringTermVal f (StringTerm t a) = (\t' -> StringTerm t' a) <$> f t

data BoolTerm a = BoolTerm !Bool a
                deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

boolTermVal :: Lens' (BoolTerm a) Bool
boolTermVal f (BoolTerm b a) = (\b' -> BoolTerm b' a) <$> f b

data IntTerm a = IntTerm !Int a
               deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

intTermVal :: Lens' (IntTerm a) Int
intTermVal f (IntTerm i a) = (\i' -> IntTerm i' a) <$> f i

data Term a = TBool !(BoolTerm a)
            | TInt !(IntTerm a)
            | TString !(StringTerm a)
            | TStringList [StringTerm a] a
            deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data Expr a = ETerm (Term a)
            | ECall (Identifier a) [Expr a]
            | EAccess (Identifier a) (Identifier a)
            deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data TargetPath a = LocalTarget (Identifier a)
                  | GlobalTarget FilePath (Identifier a)
                  deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data ScopeOp = ScopeAssign
             | ScopeAppend
             | ScopeDiff
             deriving (Show, Eq, Ord)

data ScopeAct a = ScopeAct !(Identifier a) !ScopeOp !(Expr a)
                deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data Scope a = Scope [ScopeAct a] a
             deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data Command a = Command
   { _commandName   :: Identifier a
   , _commandLabel  :: Identifier a
   , _commandConfig :: (Scope a)
   } deriving (Show, Functor, Traversable, Foldable)

data Type = TypeAny
          | TypeInt
          | TypeString
          | TypeScope
          | TypeList Type
          deriving (Show, Eq, Ord)

data TopItem a = TopTarget !(Command a)
               | TopInclude !FilePath a
               deriving (Show, Functor, Traversable, Foldable)

$(makeClassy ''Command)

instance HasDelta (Identifier Delta) where
  delta (Identifier _ d) = d

instance HasDelta (StringTerm Delta) where
  delta (StringTerm _ d) = d

instance HasDelta (IntTerm Delta) where
  delta (IntTerm _ d) = d

instance HasDelta (BoolTerm Delta) where
  delta (BoolTerm _ d) = d

instance HasDelta (Term Delta) where
  delta (TBool b) = delta b
  delta (TInt i) = delta i
  delta (TString s) = delta s
  delta (TStringList _ d) = d

instance HasDelta (Expr Delta) where
  delta v = case v of
    ETerm t -> delta t
    ECall i _ -> delta i
    EAccess i _ -> delta i
