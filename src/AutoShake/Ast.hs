{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module AutoShake.Ast
       ( Identifier(..)
       , Term(..)
       , StringTerm(..)
       , BoolTerm(..)
       , IntTerm(..)
       , TargetPath(..)
       , Target(..)
       , Expr(..)
       , ScopeOp(..)
       , ScopeAct(..)
       , Scope(..)
       , TopItem(..)
       , Type(..)
       ) where

import           Data.HashMap.Strict (HashMap)
import           Data.Text           (Text)
import           System.FilePath

data Identifier a = Identifier !Text a
                  deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data StringTerm a = StringTerm !Text a
                  deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data BoolTerm a = BoolTerm !Bool a
                deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data IntTerm a = IntTerm !Int a
               deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data Term t a = TBool !(BoolTerm a)
              | TInt !(IntTerm a)
              | TString !(StringTerm a)
              | TStringList [StringTerm a] a
              deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data Expr t a = ETerm t (Term t a)
              | ECall t (Identifier a) [Expr t a]
             deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data TargetPath a = LocalTarget (Identifier a)
                  | GlobalTarget FilePath (Identifier a)
                  deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data ScopeOp = ScopeAssign
             | ScopeDiff
             | ScopeUnion
             deriving (Show, Eq, Ord)

data ScopeAct t a = ScopeAct !(Identifier a) !ScopeOp !(Expr t a)
             deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data Scope t a = Scope [ScopeAct t a]
             deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data Target t a = Target
   { _targetType   :: Identifier a
   , _targetName   :: Identifier a
   , _targetConfig :: (Scope t a)
   } deriving (Show, Functor, Traversable, Foldable)

data Type = TypeAny
          | TypeInt
          | TypeString
          | TypeScope
          | TypeList Type
          deriving (Show, Eq, Ord)

data TopItem t a = TopTarget !(Target t a)
                 | TopInclude !FilePath a
                 deriving (Show, Functor, Traversable, Foldable)
