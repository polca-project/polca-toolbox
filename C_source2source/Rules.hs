-- Copyright (c) 2013-2016, The IMDEA Software Institute and
-- Copyright (c) 2013-2016, Universidad Politécnica de Madrid

-- See LICENSE.txt and AUTHORS.txt for licensing and authorship

{-# OPTIONS_GHC -w #-}

module Rules where 

import RulesLib

import Language.C
import Language.C.Data.Ident

import Data.Generics

nameRulesAll = []
dictRulesAll = []
nameRules = []
dictRules = []
stmtRules :: [TransState -> CStatAnn -> [((String, CStatAnn, CStatAnn), TransState,[(String, CStatAnn)])]]
stmtRules = []
exprRules :: [TransState -> CExprAnn -> [((String, CExprAnn, CExprAnn), TransState,[(String, CStatAnn)])]]
exprRules = []
pragmasRules = []

