{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Show where

import GHC.Hs

deriving instance (Show a) => Show (SrcSpanAnn' a)
deriving instance Show AddEpAnn
deriving instance Show AnnContext
deriving instance Show AnnList
deriving instance Show AnnListItem
deriving instance Show AnnParen
deriving instance Show AnnPragma
deriving instance Show AnnSortKey
deriving instance (Show ann) => Show (EpAnn ann)
deriving instance Show EpAnnComments
deriving instance Show EpaLocation
deriving instance Show NameAdornment
deriving instance Show NameAnn
deriving instance Show NoEpAnns
deriving instance Show ParenType
deriving instance Show TokenLocation
deriving instance Show TrailingAnn
deriving instance Show (HsDecl GhcPs)

deriving instance Show HsExpr (i :: Type)
deriving instance Show HsSplice (i :: Type)
deriving instance Show MatchGroup (a :: Type) (body :: Type)
deriving instance Show GRHSs (a :: Type) (body :: Type)
deriving instance Show Pat p
deriving instance Show HsRecFields p arg         -- A bunch of record fields
deriving instance Show HsFieldBind lhs rhs = HsFieldBind {
deriving instance Show Pat (i :: Type)
deriving instance Show HsExpr p
deriving instance Show DotFieldOcc p
deriving instance Show HsPragE p
deriving instance Show HsTupArg id
deriving instance Show LamCaseVariant
deriving instance Show HsCmd id
deriving instance Show HsArrAppType
deriving instance Show HsCmdTop p
deriving instance Show MatchGroup p body
deriving instance Show MatchGroupTc
deriving instance Show Match p body
deriving instance Show GRHSs p body
deriving instance Show GRHS p body = GRHS (XCGRHS p body)
deriving instance Show StmtLR idL idR body -- body should always be (LHs**** idR)
deriving instance Show TransForm   -- The 'f' below is the 'using' function, 'e' is the by function
deriving instance Show ParStmtBlock idL idR
deriving instance Show ApplicativeArg idL
deriving instance Show HsSplice id
deriving instance Show SpliceDecoration
deriving instance Show HsSplicedThing id
deriving instance Show UntypedSpliceFlavour
deriving instance Show HsQuote p
deriving instance Show ArithSeqInfo id
deriving instance Show HsMatchContext p
deriving instance Show HsStmtContext p
deriving instance Show HsArrowMatchContext
deriving instance Show HsDoFlavour
deriving instance Show HsLocalBindsLR idL idR
deriving instance Show HsValBindsLR idL idR
deriving instance Show HsBindLR idL idR
deriving instance Show PatSynBind idL idR
deriving instance Show HsIPBinds id
deriving instance Show IPBind id
deriving instance Show Sig pass
deriving instance Show FixitySig pass = FixitySig (XFixitySig pass) [LIdP pass] Fixity
deriving instance Show RecordPatSynField pass
deriving instance Show HsPatSynDir id
deriving instance Show HsLit x
deriving instance Show HsOverLit p
deriving instance Show OverLitVal
deriving instance Show HsDecl p
deriving instance Show HsGroup p
deriving instance Show SpliceDecl p
deriving instance Show TyClDecl pass
deriving instance Show FunDep pass
deriving instance Show DataDeclRn = DataDeclRn
deriving instance Show TyClGroup pass  -- See Note [TyClGroups and dependency analysis]
deriving instance Show FamilyResultSig pass = -- see Note [FamilyResultSig]
deriving instance Show FamilyDecl pass = FamilyDecl
deriving instance Show InjectivityAnn pass
deriving instance Show FamilyInfo pass
deriving instance Show HsDataDefn pass   -- The payload of a data type defn
deriving instance Show HsDerivingClause pass
deriving instance Show DerivClauseTys pass
deriving instance Show StandaloneKindSig pass
deriving instance Show NewOrData
deriving instance Show ConDecl pass
deriving instance Show HsConDeclGADTDetails pass
deriving instance Show TyFamInstDecl pass
deriving instance Show FamEqn pass rhs
deriving instance Show ClsInstDecl pass
deriving instance Show InstDecl pass  -- Both class and family instances
deriving instance Show DerivDecl pass = DerivDecl
deriving instance Show DerivStrategy pass
deriving instance Show DefaultDecl pass
deriving instance Show ForeignDecl pass
deriving instance Show ForeignImport = -- import of a C entity
deriving instance Show CImportSpec = CLabel    CLabelString     -- import address of a C label
deriving instance Show ForeignExport = CExport  (Located CExportSpec) -- contains the calling
deriving instance Show RuleDecls pass = HsRules { rds_ext   :: XCRuleDecls pass
deriving instance Show RuleDecl pass
deriving instance Show HsRuleRn = HsRuleRn NameSet NameSet -- Free-vars from the LHS and RHS
deriving instance Show RuleBndr pass
deriving instance Show DocDecl pass
deriving instance Show WarnDecls pass = Warnings { wd_ext      :: XWarnings pass
deriving instance Show WarnDecl pass = Warning (XWarning pass) [LIdP pass] (WarningTxt pass)
deriving instance Show AnnDecl pass = HsAnnotation
deriving instance Show AnnProvenance pass = ValueAnnProvenance (LIdP pass)
deriving instance Show RoleAnnotDecl pass
deriving instance Show NoExtField = NoExtField
deriving instance Show DataConCantHappen
deriving instance Show HsToken (tok :: Symbol) = HsTok
deriving instance Show HsUniToken (tok :: Symbol) (utok :: Symbol) = HsNormalTok | HsUnicodeTok
deriving instance Show HsForAllTelescope pass
deriving instance Show LHsQTyVars pass   -- See Note [HsType binders]
deriving instance Show HsOuterTyVarBndrs flag pass
deriving instance Show HsWildCardBndrs pass thing
deriving instance Show HsPatSigType pass
deriving instance Show HsPSRn = HsPSRn
deriving instance Show HsSigType pass
deriving instance Show HsTyVarBndr flag pass
deriving instance Show HsType pass
deriving instance Show HsTyLit
deriving instance Show HsArrow pass
deriving instance Show HsLinearArrowTokens pass
deriving instance Show HsScaled pass a = HsScaled (HsArrow pass) a
deriving instance Show HsTupleSort = HsUnboxedTuple
deriving instance Show ConDeclField pass  -- Record fields have Haddock docs on them
deriving instance Show HsConDetails tyarg arg rec
deriving instance Show HsArg tm ty
deriving instance Show FieldOcc pass
deriving instance Show AmbiguousFieldOcc pass
