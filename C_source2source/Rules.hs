{-# OPTIONS_GHC -w #-}

module Rules where 

import RulesLib

import Language.C
import Language.C.Data.Ident

import Data.Generics

nameRules = []
dictRules = []
stmtRules :: [TransState -> CStatAnn -> [((String, CStatAnn, CStatAnn), TransState,[(String, CStatAnn)])]]
stmtRules = []
exprRules :: [TransState -> CExprAnn -> [((String, CExprAnn, CExprAnn), TransState,[(String, CStatAnn)])]]
exprRules = []
pragmasRules = []

