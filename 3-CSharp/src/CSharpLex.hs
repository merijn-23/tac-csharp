module CSharpLex where

import Data.Char
import Control.Monad (guard)
import ParseLib.Abstract
import Prelude hiding ((<$), (<*), (*>), sequence)

data Token = POpen    | PClose      -- parentheses     ()
           | SOpen    | SClose      -- square brackets []
           | COpen    | CClose      -- curly braces    {}
           | Comma    | Semicolon
           | KeyIf    | KeyElse
           | KeyWhile | KeyReturn
           | KeyTry   | KeyCatch
           | KeyClass | KeyVoid
           | KeyFor
           | StdType   String       -- the 8 standard types
           | Operator  String       -- the 15 operators
           | UpperId   String       -- uppercase identifiers
           | LowerId   String       -- lowercase identifiers
           | ConstInt  Int
           | ConstBool Bool
           | ConstChar Char
           deriving (Eq, Show)

----- Begin Lexer -----
lexicalScanner :: Parser Char [Token]
lexicalScanner = lexWhiteSpace *> greedy (lexComments *> lexToken <* lexWhiteSpace)
                                        <* lexComments <* eof

lexToken :: Parser Char Token
lexToken = greedyChoice
             [ lexTerminal
             , lexEnum StdType stdTypes
             , lexEnum Operator operators
             , lexConstInt
             , lexConstBool
             , lexConstChar
             , lexLowerId
             , lexUpperId
             ]

lexComments :: Parser Char [String]
lexComments = greedy (lexComment <* lexWhiteSpace)

lexComment :: Parser Char String
lexComment = token "//" <* greedy (satisfy (\c -> c /= '\n'))

lexTerminal :: Parser Char Token
lexTerminal = choice [t <$ keyword s | (t,s) <- terminals]
  where
    terminals :: [(Token, String)]
    terminals =
      [ ( POpen     , "("      )
      , ( PClose    , ")"      )
      , ( SOpen     , "["      )
      , ( SClose    , "]"      )
      , ( COpen     , "{"      )
      , ( CClose    , "}"      )
      , ( Comma     , ","      )
      , ( Semicolon , ";"      )
      , ( KeyIf     , "if"     )
      , ( KeyElse   , "else"   )
      , ( KeyWhile  , "while"  )
      , ( KeyFor    , "for"    )
      , ( KeyReturn , "return" )
      , ( KeyTry    , "try"    )
      , ( KeyCatch  , "catch"  )
      , ( KeyClass  , "class"  )
      , ( KeyVoid   , "void"   )
      ]


lexEnum :: (String -> Token) -> [String] -> Parser Char Token
lexEnum f xs = f <$> choice (map keyword xs)

stdTypes :: [String]
stdTypes = ["int", "long", "double", "float", "byte", "short", "bool", "char"]
operators :: [String]
operators = ["+", "-", "*", "/", "%", "&&", "||", "^", "<=", "<", ">=", ">", "==", "!=", "="]

lexConstInt :: Parser Char Token
lexConstInt = ConstInt . read <$> greedy1 (satisfy isDigit)

lexConstBool :: Parser Char Token
lexConstBool = ConstBool <$> ( True <$ token "true"
                           <|> False <$ token "false")

lexConstChar :: Parser Char Token
lexConstChar = ConstChar <$> (symbol '\'' *> anySymbol <* symbol '\'')

lexLowerId :: Parser Char Token
lexLowerId = (\x xs -> LowerId (x:xs)) <$> satisfy isLower <*> greedy (satisfy isAlphaNum)

lexUpperId :: Parser Char Token
lexUpperId = (\x xs -> UpperId (x:xs)) <$> satisfy isUpper <*> greedy (satisfy isAlphaNum)


lexWhiteSpace :: Parser Char String
lexWhiteSpace = greedy (satisfy isSpace)

keyword :: String -> Parser Char String
keyword [] = succeed ""
keyword xs@(x:_) | isLetter x = do ys <- greedy (satisfy isAlphaNum)
                                   guard (xs == ys)
                                   return ys
                 | otherwise  = token xs

greedyChoice :: [Parser s a] -> Parser s a
greedyChoice = foldr (<<|>) empty
----- End Lexer -----


----- Utilities for consuming tokens -----
sStdType :: Parser Token String
sStdType = (\(StdType x) -> x) <$> satisfy isStdType
  where isStdType (StdType _) = True
        isStdType _           = False

sUpperId :: Parser Token String
sUpperId = (\(UpperId x) -> x) <$> satisfy isUpperId
    where isUpperId (UpperId _) = True
          isUpperId _           = False

sLowerId :: Parser Token String
sLowerId = (\(LowerId x) -> x) <$> satisfy isLowerId
  where isLowerId (LowerId _) = True
        isLowerId _           = False

sConstInt :: Parser Token Int
sConstInt = (\(ConstInt x) -> x) <$> satisfy isConst
  where isConst (ConstInt  _) = True
        isConst _             = False

sConstChar :: Parser Token Char
sConstChar = (\(ConstChar x) -> x) <$> satisfy isConst
  where isConst (ConstChar _) = True
        isConst _             = False

sConstBool :: Parser Token Bool
sConstBool = (\(ConstBool x) -> x) <$> satisfy isConst
  where isConst (ConstBool _) = True
        isConst _             = False

sOperator :: Parser Token String
sOperator = (\(Operator x) -> x) <$> satisfy isOperator
  where isOperator (Operator _) = True
        isOperator _            = False

sSemi :: Parser Token Token
sSemi =  symbol Semicolon
