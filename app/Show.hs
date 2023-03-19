{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Show where

import GHC.Hs
import GHC.Types.Name.Reader (RdrName(..))
import GHC.Types.SrcLoc (GenLocated(..))
import GHC.Types.Tickish
import GHC.Unit.Types (Module(..))
import GHC.Core.Coercion.Axiom
import GHC.Data.BooleanFormula
import GHC.Types.Basic
import GHC.Types.Fixity
import GHC.Types.ForeignCall
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.SourceText
import GHC.Types.Var
import GHC.Unit.Module.Warnings

-- deriving instance Show (Bag)
-- deriving instance Show (Module)
-- deriving instance (Anno [GenLocated SrcSpanAnnA (StmtLR GhcPs GhcPs a)] ~ SrcSpanAnnL, Anno (StmtLR GhcPs GhcPs a) ~ SrcSpanAnnA, Show a) => Show (StmtLR GhcPs GhcPs a)
-- deriving instance (Show a, Show b) => Show (HsArg a b)
-- deriving instance (Show a, Show b, Show c) => Show (HsConDetails a b c)
-- deriving instance Show (AmbiguousFieldOcc GhcPs)
-- deriving instance Show (AnnDecl GhcPs)
-- deriving instance Show (AnnProvenance GhcPs)
-- deriving instance Show (ClsInstDecl GhcPs)
-- deriving instance Show (ConDecl GhcPs)
-- deriving instance Show (ConDeclField GhcPs)
-- deriving instance Show (CoreTickish)
-- deriving instance Show (DefaultDecl GhcPs)
-- deriving instance Show (DerivClauseTys GhcPs)
-- deriving instance Show (DerivDecl GhcPs)
-- deriving instance Show (DerivStrategy GhcPs)
-- deriving instance Show (DocDecl GhcPs)
-- deriving instance Show (DotFieldOcc GhcPs)
-- deriving instance Show (FamilyDecl GhcPs)
-- deriving instance Show (FamilyInfo GhcPs)
-- deriving instance Show (FamilyResultSig GhcPs)
-- deriving instance Show (FieldOcc GhcPs)
-- deriving instance Show (FixitySig GhcPs)
-- deriving instance Show (ForeignDecl GhcPs)
-- deriving instance Show (FunDep GhcPs)
-- deriving instance Show (GhcPs)
-- deriving instance Show (HsArrow GhcPs)
-- deriving instance Show (HsBindLR GhcPs GhcPs)
-- deriving instance Show (HsCmdTop GhcPs)
-- deriving instance Show (HsConDeclGADTDetails GhcPs)
-- deriving instance Show (HsDataDefn GhcPs)
-- deriving instance Show (HsDecl GhcPs)
-- deriving instance Show (HsDerivingClause GhcPs)
-- deriving instance Show (HsExpr GhcPs)
-- deriving instance Show (HsFieldBind GhcPs GhcPs)
-- deriving instance Show (HsForAllTelescope GhcPs)
-- deriving instance Show (HsGroup GhcPs)
-- deriving instance Show (HsLinearArrowTokens GhcPs)
-- deriving instance Show (HsLocalBindsLR GhcPs GhcPs)
-- deriving instance Show (HsMatchContext GhcPs)
-- deriving instance Show (HsOverLit GhcPs)
-- deriving instance Show (HsPatSigType GhcPs)
-- deriving instance Show (HsPragE GhcPs)
-- deriving instance Show (HsQuote GhcPs)
-- deriving instance Show (HsSigType GhcPs)
-- deriving instance Show (HsSplice GhcPs)
-- deriving instance Show (HsStmtContext GhcPs)
-- deriving instance Show (HsType GhcPs)
-- deriving instance Show (HsValBindsLR GhcPs GhcPs)
-- deriving instance Show (InjectivityAnn GhcPs)
-- deriving instance Show (InstDecl GhcPs)
-- deriving instance Show (LHsQTyVars GhcPs)
-- deriving instance Show (ParStmtBlock GhcPs GhcPs)
-- deriving instance Show (PatSynBind GhcPs GhcPs)
-- deriving instance Show (RecordPatSynField GhcPs)
-- deriving instance Show (RoleAnnotDecl GhcPs)
-- deriving instance Show (RuleBndr GhcPs)
-- deriving instance Show (RuleDecl GhcPs)
-- deriving instance Show (RuleDecls GhcPs)
-- deriving instance Show (Sig GhcPs)
-- deriving instance Show (SpliceDecl GhcPs)
-- deriving instance Show (StandaloneKindSig GhcPs)
-- deriving instance Show (TyClDecl GhcPs)
-- deriving instance Show (TyClGroup GhcPs)
-- deriving instance Show (TyFamInstDecl GhcPs)
-- deriving instance Show (WarnDecl GhcPs)
-- deriving instance Show (WarnDecls GhcPs)
-- deriving instance Show AddEpAnn
-- deriving instance Show AnnContext
-- deriving instance Show AnnList
-- deriving instance Show AnnListItem
-- deriving instance Show AnnParen
-- deriving instance Show AnnPragma
-- deriving instance Show AnnSortKey
-- deriving instance Show CImportSpec
-- deriving instance Show DataConCantHappen
-- deriving instance Show DataDeclRn
-- deriving instance Show EpAnnComments
-- deriving instance Show EpaLocation
-- deriving instance Show ForeignExport
-- deriving instance Show ForeignImport
-- deriving instance Show HsArrAppType
-- deriving instance Show HsArrowMatchContext
-- deriving instance Show HsDoFlavour
-- deriving instance Show HsPSRn
-- deriving instance Show HsRuleRn
-- deriving instance Show HsTupleSort
-- deriving instance Show HsTyLit
-- deriving instance Show LamCaseVariant
-- deriving instance Show MatchGroupTc
-- deriving instance Show NameAdornment
-- deriving instance Show NameAnn
-- deriving instance Show NewOrData
-- deriving instance Show NoEpAnns
-- deriving instance Show NoExtField
-- deriving instance Show OverLitVal
-- deriving instance Show ParenType
-- deriving instance Show RdrName
-- deriving instance Show TokenLocation
-- deriving instance Show TrailingAnn
-- deriving instance Show TransForm
-- deriving instance Show UntypedSpliceFlavour
-- deriving instance Show a => Show (ApplicativeArg a)
-- deriving instance Show a => Show (ArithSeqInfo a)
-- deriving instance Show a => Show (EpAnn a)
-- deriving instance Show a => Show (FamEqn GhcPs a)
-- deriving instance Show a => Show (GRHS GhcPs a)
-- deriving instance Show a => Show (GRHSs GhcPs a)
-- deriving instance Show a => Show (HsCmd a)
-- deriving instance Show a => Show (HsIPBinds a)
-- deriving instance Show a => Show (HsLit a)
-- deriving instance Show a => Show (HsOuterTyVarBndrs a GhcPs)
-- deriving instance Show a => Show (HsPatSynDir a)
-- deriving instance Show a => Show (HsRecFields GhcPs a)
-- deriving instance Show a => Show (HsScaled GhcPs a)
-- deriving instance Show a => Show (HsSplicedThing a)
-- deriving instance Show a => Show (HsTupArg a)
-- deriving instance Show a => Show (HsTyVarBndr a GhcPs)
-- deriving instance Show a => Show (HsWildCardBndrs GhcPs a)
-- deriving instance Show a => Show (IPBind a)
-- deriving instance Show a => Show (Match GhcPs a)
-- deriving instance Show a => Show (MatchGroup GhcPs a)
-- deriving instance Show a => Show (Pat a)
-- deriving instance Show a => Show (SrcSpanAnn' a)
-- deriving instance Show (DataFamInstDecl GhcPs)
-- deriving instance Show (BooleanFormula (GenLocated SrcSpanAnnN RdrName))
-- deriving instance Show (WarningTxt GhcPs)
-- deriving instance Show (HsFieldBind (GenLocated (SrcAnn NoEpAnns) (AmbiguousFieldOcc GhcPs)) (GenLocated SrcSpanAnnA (HsExpr GhcPs)))
-- deriving instance Show (HsUniToken "->" "\8594")
-- deriving instance Show (WithHsDocIdentifiers HsDocString GhcPs)
-- deriving instance Show AnnFieldLabel
-- deriving instance Show Fixity
-- deriving instance Show Role
-- deriving instance Show OverlapMode
-- deriving instance Show TopLevelFlag
-- deriving instance Show LexicalFixity
-- deriving instance Show CCallConv
-- deriving instance Show CCallTarget
-- deriving instance Show CExportSpec
-- deriving instance Show CType
-- --deriving instance Show Name
-- --deriving instance Show NameSet
-- deriving instance Show StringLiteral
-- deriving instance Show Specificity
-- deriving instance Show HsRuleAnn
-- deriving instance Show SpliceExplicitFlag
-- deriving instance Show ThModFinalizers
-- deriving instance Show XViaStrategyPs
-- 
--    • Could not deduce (Show (Anno (GRHS GhcPs a)))
--    • Could not deduce (Show (Anno (Match GhcPs a)))
--    • Could not deduce (Show (HsExpr a))
--    • Could not deduce (Show (HsFieldBind (GenLocated (SrcAnn NoEpAnns) (FieldOcc GhcPs)) a))
--    • Could not deduce (Show (HsRecFields a (XRec a (Pat a))))
--    • Could not deduce (Show (MatchGroup a (XRec a (HsCmd a))))
--    • Could not deduce (Show (MatchGroup a (XRec a (HsExpr a))))
--    • Could not deduce (Show (XHsChar a))
--    • Could not deduce (Show (XRec a (HsExpr a)))
--    • Could not deduce (Show (XRec a (IPBind a)))
--    • Could not deduce (Show (XRec a (StmtLR a a (XRec a (HsExpr a)))))
--    • Could not deduce (Show GHC.Types.Fixity.LexicalFixity)
--    • Could not deduce (Show GrhsAnn) arising from a use of ‘showsPrec’
