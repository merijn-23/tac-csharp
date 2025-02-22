module CSharpAlgebra where

import CSharpLex
import CSharpGram

{-
  Only modify this file when you change the AST in CSharpGram.hs
-}

type CSharpAlgebra clas memb stat expr             --
  = (  String -> [memb] -> clas                    --  Class = Class String [Member]
                                                   --
    , ( Decl                              -> memb  --  Member = MemberD Decl
      , Type -> String -> [Decl] -> stat  -> memb  --         | MemberM Type String [Decl] Stat
      )                                            --
                                                   --
    , ( Decl                  -> stat              --  Stat = StatDecl   Decl
      , expr                  -> stat              --       | StatExpr   Expr
      , expr -> stat -> stat  -> stat              --       | StatIf     Expr Stat Stat
      , expr -> stat          -> stat              --       | StatWhile  Expr Stat
      , expr                  -> stat              --       | StatReturn Expr
      , [stat]                -> stat              --       | StatBlock  [Stat]
      )                                            --
                                                   --
    , ( Int                     -> expr            --  Expr = ExprConstInt   Int
      , Bool                    -> expr            --       | ExprConstBool  Bool
      , Char                    -> expr            --       | ExprConstChar  Char
      , String                  -> expr            --       | ExprVar    String
      , String -> expr -> expr  -> expr            --       | ExprOper   String Expr Expr
      , String -> [expr]        -> expr            --       | ExprMeth   String [Expr]
      )                                            --
    )                                              --

foldCSharp :: CSharpAlgebra clas memb stat expr -> Class -> clas
foldCSharp (c, (md,mm), (sd,se,si,sw,sr,sb), (eci,ecb,ecc,ev,eo,em)) = fClas
  where
    fClas (Class      t ms)     = c  t (map fMemb ms)
    fMemb (MemberD    d)        = md d
    fMemb (MemberM    t m ps s) = mm t m ps (fStat s)
    fStat (StatDecl   d)        = sd d
    fStat (StatExpr   e)        = se (fExpr e)
    fStat (StatIf     e s1 s2)  = si (fExpr e) (fStat s1) (fStat s2)
    fStat (StatWhile  e s1)     = sw (fExpr e) (fStat s1)
    fStat (StatReturn e)        = sr (fExpr e)
    fStat (StatBlock  ss)       = sb (map fStat ss)
    fExpr (ExprConstInt con)    = eci con
    fExpr (ExprConstBool con)   = ecb con
    fExpr (ExprConstChar con)   = ecc con
    fExpr (ExprVar    var)      = ev var
    fExpr (ExprOper   op e1 e2) = eo op (fExpr e1) (fExpr e2)
    fExpr (ExprMeth   s es)     = em s (map fExpr es)
