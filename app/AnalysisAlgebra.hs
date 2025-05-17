{-# LANGUAGE InstanceSigs #-}
module AnalysisAlgebra where

import AbstractSyntax
import Algebra

{--
analyse = cFolder analysysAlgebra

data AnalysisErrors = NoErrors
                        | ScopeError String
                        | TypeError String
                        | ScopeAndTypeError String
    deriving Eq

instance Show AnalysisErrors where
    show :: AnalysisErrors -> String
    show NoErrors = "No errors"
    show (ScopeError x) = "Scope error detected: " ++ x
    show (TypeError x) = "Type error detected: " ++ x
    show (ScopeAndTypeError x) = "Scope and Type errors detected: " ++ x

analysysAlgebra :: CAlgebra AnalysisErrors me s exp v [(String,VarType)]
analysysAlgebra = CAlgebra

--}