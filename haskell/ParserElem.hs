module ParserElem (
    spaces
  , lexstr
  , identifier
  , integer
  , float
  , colon
  , comma
  , semicol
  , parens
  , braces
  , brackets
  , angles
  )
where

------------------------------------------------------------

import           Text.Parsec hiding (spaces)
import qualified Text.Parsec.Token as T
import           Text.Parsec.Language (haskellDef)

------------------------------------------------------------

lexer  = T.makeTokenParser haskellDef
lexeme = T.lexeme lexer

spaces = T.whiteSpace lexer
lexstr = lexeme . string

identifier = T.identifier lexer
integer    = T.integer lexer
float      = do
  t <- optionMaybe (try $ T.float lexer)
  case t of
    Just t -> return t
    Nothing -> fmap fromInteger integer

dot      = T.dot lexer      -- .
colon    = T.colon lexer    -- :
comma    = T.comma lexer    -- ,
semicol  = T.semi lexer     -- ;
parens   = T.parens lexer   -- ()
braces   = T.braces lexer   -- {}
brackets = T.brackets lexer -- []
angles   = T.angles lexer   -- <>

