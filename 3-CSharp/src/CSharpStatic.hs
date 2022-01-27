module CSharpStatic where

import Prelude hiding (LT, GT, EQ)
import qualified Data.Map as M
import qualified Data.List as L

import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM

import Data.Char

-- Determines how many declarations and parameters each function has
-- Here we have (Label, (# of local decls, # of params)) as return type
staticAlgebra :: CSharpAlgebra [(String, (Int, Int))] (String, (Int, Int)) Int Int
staticAlgebra =
    ( staticClass
    , staticMember
    , staticStatement
    , staticExpr
    )

staticClass _ ms = filter (/= ("", (0,0))) ms
staticMember = (const ("", (0,0)), (\_ name ds s -> (name,(s,length ds))))
staticStatement = (const 1, const 0, (\_ s1 s2 -> s1+s2), flip const, const 0, sum)
staticExpr = (const 0, const 0, const 0, const 0, const . const . const 0, const . const 0)
