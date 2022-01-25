module CSharpCode where

import Prelude hiding (LT, GT, EQ)
import qualified Data.Map as M
import qualified Data.List as L

import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM

import Data.Char

{-
  This file contains a starting point for the code generation which should handle very simple programs.
-}

-- The types that we generate for each datatype: Our type variables for the algebra
type C = Env -> Code                   -- Class
type M = Env -> Code                   -- Member
type S = Env -> (Code, Env)            -- Statement
type E = Env -> ValueOrAddress -> Code -- Expression
type Env = M.Map String Int

codeAlgebra :: CSharpAlgebra C M S E--(Env -> C) (Env -> M) (Env -> S) (Env -> E)
codeAlgebra =
    ( codeClass
    , codeMember
    , codeStatement
    , codeExpr
    )

codeClass :: String -> [M] -> C
codeClass c ms env = [Bsr "main", HALT] ++ (concat $ map (\m -> m env) ms)

codeMember = (fMembDecl, fMembMeth)
  where
    fMembDecl :: Decl -> M
    fMembDecl d env = [] -- insert for exercise 11

    fMembMeth :: Type -> String -> [Decl] -> S -> M
    fMembMeth t x ps s env = [LABEL x] ++ (fst $ s env) ++ [RET]

codeStatement = (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatReturn, fStatBlock)
  where
    fStatDecl :: Decl -> S
    fStatDecl (Decl _ s) env = ([], M.insert s address env)
      where
        address = if null elems then 0 else ((maximum elems) + 1)
        elems = M.elems env

    fStatExpr :: E -> S
    fStatExpr e env = (e env Value ++ [pop], env)

    fStatIf :: E -> S -> S -> S
    fStatIf e s1 s2 env =
      (c ++ [BRF (n1 + 2)] ++ b1 ++ [BRA n2] ++ b2, env) -- paired with old environment
        where
            c        = e env Value
            (n1, n2) = (codeSize b1, codeSize b2) -- size of blocks of code
            (b1, b2) = ((fst $ s1 env), (fst $ s2 env)) -- block of code

    fStatWhile :: E -> S -> S
    fStatWhile e s env = ([BRA n] ++ (fst $ s env) ++ c ++ [BRT (-(n + k + 2))], env)
        where
            c = e env Value
            (n, k) = ((codeSize $ fst $ s env), (codeSize c))

    fStatReturn :: E -> S
    fStatReturn e env = (e env Value ++ [pop] ++ [RET], env)

    fStatBlock :: [S] -> S
    fStatBlock [] env = ([], env)
    fStatBlock (x:xs) env = L.foldl' foldBlock (x env) xs -- xs :: [Env -> S]

    foldBlock :: (Code, Env) -> S -> (Code, Env)
    foldBlock (code, env) s = (code ++ code', env')
      where
        (code', env') = s env

codeExpr = (fExprConInt, fExprConBool, fExprConChar, fExprVar, fExprOp)
  where
    fExprConInt :: Int -> E
    fExprConInt n va _ = [LDC n]

    fExprConBool :: Bool -> E
    fExprConBool n va _ = [LDC $ fromEnum n]

    fExprConChar :: Char -> E
    fExprConChar n va _ = [LDC $ ord n]

    fExprVar :: String -> E
    fExprVar x env va = let res = M.lookup x env in
                                case res of
                                  Nothing  -> error $ "Variable " ++ x ++ " not in scope!"
                                  Just loc ->
                                    case va of
                                      Value    ->  [LDL  loc]
                                      Address  ->  [LDLA loc]

    fExprOp :: String -> E -> E -> E
    fExprOp "=" e1 e2 env _  = e2 env Value ++ [LDS 0] ++ e1 env Address ++ [STA 0]
    fExprOp "||" e1 e2 env _ = fExprOpLazily BRT OR e1 e2 env Value
    fExprOp "&&" e1 e2 env _ = fExprOpLazily BRF AND e1 e2 env Value
    fExprOp op  e1 e2 env _  = e1 env Value ++ e2 env Value ++ [opCodes M.! op]
      where
        opCodes :: M.Map String Instr
        opCodes = M.fromList [ ("+", ADD), ("-",  SUB), ("*", MUL), ("/", DIV), ("%", MOD)
                             , ("<=", LE), (">=",  GE), ("<",  LT), (">",  GT), ("==", EQ)
                             , ("!=", NE), ("^", XOR)
                             ]

    -- first instruction is the branch condition (BRT or BRF), second one is operator in case laziness is not applicable
    fExprOpLazily :: (Int -> Instr) -> Instr -> E -> E -> E
    fExprOpLazily brn op e1 e2 env _ = c1 ++ [LDL 1, brn (n2 + 1)] ++ c2 ++ [op]
      where
        n2 = codeSize c2
        (c1, c2) = (e1 env Value, e2 env Value)

-- | Whether we are computing the value of a variable, or a pointer to it
data ValueOrAddress = Value | Address
  deriving Show
