module CSharpGram where

import ParseLib.Abstract hiding (braced, bracketed, parenthesised)
import CSharpLex
import Prelude hiding ((<$), (<*), (*>), sequence)

data Class = Class String [Member]
           deriving Show

data Member = MemberD Decl
            | MemberM Type String [Decl] Stat
            deriving Show

data Stat = StatDecl   Decl
          | StatExpr   Expr
          | StatIf     Expr Stat Stat
          | StatWhile  Expr Stat
          | StatReturn Expr
          | StatBlock  [Stat]
          deriving Show

data Expr = ExprConstInt  Int
          | ExprConstChar Char
          | ExprConstBool Bool
          | ExprVar    String
          | ExprOper   String Expr Expr
          deriving Show

data Decl = Decl Type String
          deriving Show

data Type = TypeVoid
          | TypePrim  String
          | TypeObj   String
          deriving (Eq,Show)


pClass :: Parser Token Class
pClass = Class <$ symbol KeyClass <*> sUpperId <*> braced (many pMember)

pMember :: Parser Token Member
pMember =  MemberD <$> pDeclSemi
       <|> pMeth

pMeth :: Parser Token Member
pMeth = MemberM <$> methRetType <*> sLowerId <*> methArgList <*> pBlock
    where
        methRetType = pType <|> TypeVoid <$ symbol KeyVoid
        methArgList = parenthesised (option (listOf pDecl (symbol Comma)) [])

pBlock :: Parser Token Stat
pBlock = StatBlock <$> braced (many pStatDecl)

pStatDecl :: Parser Token Stat
pStatDecl =  pStat
         <|> StatDecl <$> pDeclSemi

pStat :: Parser Token Stat
pStat =  StatExpr <$> pExpr <*  sSemi
     <|> StatIf     <$ symbol KeyIf     <*> parenthesised pExpr <*> pStat <*> optionalElse
     <|> StatWhile  <$ symbol KeyWhile  <*> parenthesised pExpr <*> pStat
     <|> StatReturn <$ symbol KeyReturn <*> pExpr               <*  sSemi
     <|> pBlock
     where optionalElse = option (symbol KeyElse *> pStat) (StatBlock [])

pExprSimple :: Parser Token Expr
pExprSimple =  ExprConstInt <$> sConstInt
           <|> ExprConstChar <$> sConstChar
           <|> ExprConstBool <$> sConstBool
           <|> ExprVar   <$> sLowerId
           <|> parenthesised pExpr

-- Another approach for deciding between left and right associativity, could be:
--   add a boolean to each operator in the double list, True means it should use genl, False uses genr
--   then fold with generic gen function, which picks genl or genr based on the boolean
--   we opted for this approach because
--     a) it is simpler
--     b) "=" is our only right associative operator, so this does not introduce duplicative code
pExpr :: Parser Token Expr
pExpr = foldr (genl . map (\s -> (Operator s, ExprOper s)))
          (genr [(Operator "=", ExprOper "=")] pExprSimple) -- initial expression, after right associative "="
          [ ["||"] -- second lowest order of precendence, after "="
          , ["&&"]
          , ["^"]
          , ["==", "!="]
          , ["<", ">", "<=", ">="]
          , ["+", "-"]
          , ["*", "/", "%"] ]      -- highest order of precedence

-- From slides 04, adjusted to use String instead of Char
type Op a = (Token, a -> a -> a)
genl :: [Op a] -> Parser Token a -> Parser Token a
genl ops p = chainl p (choice (map (\(s, c) -> c <$ symbol s) ops))

genr :: [Op a] -> Parser Token a -> Parser Token a
genr ops p = chainr p (choice (map (\(s, c) -> c <$ symbol s) ops))

pDecl :: Parser Token Decl
pDecl = Decl <$> pType <*> sLowerId

pDeclSemi :: Parser Token Decl
pDeclSemi = pDecl <* sSemi

pType :: Parser Token Type
pType =  TypePrim <$> sStdType
     <|> TypeObj  <$> sUpperId


-- The `Token` equivalents to some basic parser combinators
parenthesised, bracketed, braced :: Parser Token b -> Parser Token b
parenthesised p = pack (symbol POpen) p (symbol PClose) --(p)
bracketed     p = pack (symbol SOpen) p (symbol SClose) --[p]
braced        p = pack (symbol COpen) p (symbol CClose) --{p}
