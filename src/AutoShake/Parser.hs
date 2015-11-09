{-# LANGUAGE OverloadedStrings #-}
module AutoShake.Parser
       ( parseFile
       , parseByteString
       ) where

import           Control.Applicative       ((<|>))
import           Control.Monad.IO.Class    (MonadIO)

import           Data.ByteString           (ByteString)

import           Text.Trifecta.Combinators (position)
import           Text.Trifecta.Delta       (Delta (..))
import           Text.Trifecta.Parser      (Parser)
import qualified Text.Trifecta.Parser      as Trifecta
import           Text.Trifecta.Result      (Result)

import           Text.Parser.Combinators   (eof, many, (<?>))
import           Text.Parser.Token         (IdentifierStyle, braces, brackets,
                                            colon, comma, commaSep, ident,
                                            integer, parens, stringLiteral,
                                            symbol, symbolic, textSymbol)
import           Text.Parser.Token.Style   (emptyIdents)

import           AutoShake.Ast             (BoolTerm (..), Command (..),
                                            Expr (..), Identifier (..),
                                            IntTerm (..), Scope (..),
                                            ScopeAct (..), ScopeOp (..),
                                            StringTerm (..), TargetPath (..),
                                            Term (..), TopItem (..))

identifier :: Parser (Identifier Delta)
identifier = flip Identifier <$> position <*> ident emptyIdents
             <?> "identifier"

boolTerm :: Parser (BoolTerm Delta)
boolTerm = flip BoolTerm <$> position <*> ( True <$ symbol "true" <|> False <$ symbol "false")

stringTerm :: Parser (StringTerm Delta)
stringTerm = flip StringTerm <$> position <*> stringLiteral
             <?> "string"

intTerm :: Parser (IntTerm Delta)
intTerm = flip (IntTerm . fromInteger) <$> position <*> integer
          <?> "integer"

stringList :: Parser [StringTerm Delta]
stringList = brackets $ commaSep stringTerm
             <?> "string List"

value :: Parser (Term Delta)
value = TString <$> stringTerm
        <|> TInt <$> intTerm
        <|> flip TStringList <$> position <*> stringList
        <|> TBool <$> boolTerm
        <?> "value"

expr :: Parser (Expr Delta)
expr = ETerm <$> value
       <|> ECall <$> identifier <*> parens (commaSep expr)

scopeOp :: Parser ScopeOp
scopeOp = ScopeAssign <$ symbolic '='
          <|> ScopeAppend <$ symbol "+="
          <|> ScopeDiff <$ symbol "-="
          <?> "operator"


scopeAct :: Parser (ScopeAct Delta)
scopeAct = ScopeAct <$> identifier <*> scopeOp <*> expr

scope = flip Scope <$> position <*> braces (many scopeAct)

target = (\i n sc -> Command i n sc)
         <$> identifier
         <*> parens identifier
         <*> scope
         <?> "target"

topItem :: Parser (TopItem Delta)
topItem = (\pos s -> TopInclude s pos)
          <$> (position <* symbol "include") <*> stringLiteral
          <|> TopTarget <$> target

configFile :: Parser [TopItem Delta]
configFile = many topItem <* eof

parseByteString :: ByteString
                -> ByteString
                -> Text.Trifecta.Result.Result [TopItem Delta]
parseByteString fn = Trifecta.parseByteString configFile (Directed fn 0 0 0 0)

parseFile :: MonadIO m => String -> m (Maybe [TopItem Delta])
parseFile fn = Trifecta.parseFromFile configFile fn
