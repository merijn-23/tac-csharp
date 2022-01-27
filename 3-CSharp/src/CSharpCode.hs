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
type C = Env -> Code                             -- Class
type M = Env -> Code                             -- Member
type S = Env -> String -> (Code, Env)            -- Statement
type E = Env -> ValueOrAddress -> Code           -- Expression
type Env = (M.Map String (Int,Int), M.Map String Int) -- First value is number of declarations per func, second is address of local variables and parameters

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

    -- We purposely throw away the old local vars Map, to ensure that we start with an empty environment
    -- (even though the old one should always be empty, but just in case :P)
    -- procedure:
    -- Make room for local variables below MP (Link k)
    -- clean up before we RET
    fMembMeth :: Type -> String -> [Decl] -> S -> M
    fMembMeth t x ps s (fncs, _) = [LABEL x, LINK k]
                                ++ (fst $ s env' x)
                                ++ [UNLINK, STS (-n), AJS (-(n-1)), RET]
      where
        env'  = (fncs, vars')
        vars' = foldr (\(Decl _ l) m -> M.insert l (-(n+1) + M.size m) m) M.empty ps
        (k,n) = case M.lookup x fncs of
              Nothing -> error $ "Function " ++ x ++ " never declared!"
              Just res -> res

codeStatement = (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatReturn, fStatBlock)
  where
    fStatDecl :: Decl -> S
    fStatDecl (Decl _ s) (fncs, vars) _ = ([], (fncs, M.insert s address vars))
      where
        address = if null elems then 1 else max ((maximum elems) + 1)  1
        elems = M.elems vars

    fStatExpr :: E -> S
    fStatExpr e env _ = (e env Value ++ [pop], env)

    fStatIf :: E -> S -> S -> S
    fStatIf e s1 s2 env lbl =
      (c ++ [BRF (n1 + 2)] ++ b1 ++ [BRA n2] ++ b2, env) -- paired with old environment
        where
            c        = e env Value
            (n1, n2) = (codeSize b1, codeSize b2) -- size of blocks of code
            (b1, b2) = ((fst $ s1 env lbl), (fst $ s2 env lbl)) -- block of code

    fStatWhile :: E -> S -> S
    fStatWhile e s env lbl = ([BRA n] ++ (fst $ s env lbl) ++ c ++ [BRT (-(n + k + 2))], env)
        where
            c = e env Value
            (n, k) = ((codeSize $ fst $ s env lbl), (codeSize c))

    fStatReturn :: E -> S
    fStatReturn e env lbl = (e env Value ++ [STR r3] ++ [UNLINK, STS (-n), AJS (-(n-1)), RET], env)
      where
        (_,n) = case M.lookup lbl (fst env) of
              Nothing -> error $ "Function " ++ lbl ++ " never declared!"
              Just res -> res

    fStatBlock :: [S] -> S
    fStatBlock [] env lbl     = ([], env)
    fStatBlock (x:xs) env lbl = L.foldl' (foldBlock lbl) (x env lbl) xs -- xs :: [Env -> S]

    foldBlock :: String -> (Code, Env) -> S -> (Code, Env)
    foldBlock lbl (code, env) s = (code ++ code', env')
      where
        (code', env') = s env lbl

codeExpr = (fExprConInt, fExprConBool, fExprConChar, fExprVar, fExprOp, fExprMeth)
  where
    fExprConInt :: Int -> E
    fExprConInt n va _ = [LDC n]

    fExprConBool :: Bool -> E
    fExprConBool n va _ = [LDC $ fromEnum n]

    fExprConChar :: Char -> E
    fExprConChar n va _ = [LDC $ ord n]

    -- because we load our local vars with LDLA, we have to save them using STA not STL
    -- If we would return the relative position compared to the MP here, we could use STL
    -- instead we use absolute / global addresses, so use STA in fExprOp
    fExprVar :: String -> E
    fExprVar x env va = let res = M.lookup x (snd env) in
                                case res of
                                  Nothing  -> error $ "Variable " ++ x ++ " not in scope!"
                                  Just loc ->
                                    case va of
                                      Value    ->  [LDL  loc]
                                      Address  ->  [LDLA loc]

    -- slide 32 from SSM explains how to use params and local variables! MP MP MP
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


    -- TEMP added LDC 45 to simulate 'void'
    fExprMeth :: String -> [E] -> E
    fExprMeth "print" es env _ = concat (map (\e -> (e env Value) ++ [TRAP 0]) es) ++ [LDR r3]
    fExprMeth lbl es env _     = (concat (map (\e -> e env Value) es)) ++ [Bsr lbl] ++ [LDR r3]
      where
        (_,n) = case M.lookup lbl (fst env) of
                  Nothing -> error $ "Function " ++ lbl ++ " never declared!"
                  Just res -> res

-- | Whether we are computing the value of a variable, or a pointer to it
data ValueOrAddress = Value | Address
  deriving Show
