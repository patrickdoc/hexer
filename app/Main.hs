{-# LANGUAGE TemplateHaskell #-}
module Main where

import Lexer

import Language.Haskell.HLint
import GHC.Hs
import GHC.Hs.Dump
import GHC.Types.SrcLoc
import GHC.Utils.Outputable (defaultSDocContext, printSDocLn, sdocPprDebug)
import GHC.Utils.Ppr hiding (char)
import System.Environment
import System.IO

-- Flatparse
import FlatParse.Stateful hiding (Parser, runParser, string, cut)
import qualified FlatParse.Stateful as FP
import qualified Data.ByteString as B
import GHC.Data.FastString
import qualified GHC.Data.Strict as S
import GHC.Unit.Module.Name
import GHC.Types.Basic
import GHC.Types.Fixity
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader
import GHC.Types.SourceText

main :: IO ()
main = do
    args <- getArgs
    result <- case head args of
        "h" -> useHlint $ args!!1
        "f" -> useFlatparse $ args!!1
        _ -> return Nothing
    case result of
        Just m -> printModule m
        Nothing -> putStrLn "Failed to parse module"

printModule :: Located HsModule -> IO ()
printModule x = printSDocLn sdocCtx (PageMode True) stdout $ showAstDataFull (hsmodDecls (unLoc x))
  where
    sdocCtx = defaultSDocContext {
        sdocPprDebug = True
    }

----------------------------------------
-- HLint

useHlint :: String -> IO (Maybe (Located HsModule))
useHlint file = do
    (flags, _, _) <- autoSettings
    x <- parseModuleEx flags file Nothing
    case x of
        Right m -> return $ Just $ ghcModule m
        Left _ -> return Nothing

----------------------------------------
-- Flatparse

useFlatparse :: String -> IO (Maybe (Located HsModule))
useFlatparse file = do
    str <- B.readFile file
    case FP.runParser psMod str 0 str of
        Err e    -> do
            print e
            return Nothing
        OK a _ _ -> return $ Just a
        Fail     -> do
            putStrLn "uncaught parse error"
            return Nothing

psMod :: Parser (Located HsModule)
psMod = do
    $(keyword "module") <* ws
    (x, s) <- withSpan' varid
    ws
    $(keyword "where") <* ws
    d1 <- psLHsDecl <* ws
    d2 <- fDecl2
    str <- ask
    return $ mkGeneralLocated "1:1" (hsMod { hsmodName = Just (mkModName x s str)
                                           , hsmodDecls = [d1,d2]
                                           })
  where
    hsMod = HsModule
        { hsmodAnn = EpAnnNotUsed
        , hsmodName = Nothing
        , hsmodLayout = VirtualBraces 1
        , hsmodExports = Nothing
        , hsmodImports = []
        , hsmodDecls = []
        , hsmodDeprecMessage = Nothing
        , hsmodHaddockModHeader = Nothing
        }
    mkModName bs s str = let ((ar, ac), (br, bc)) = spanToRowCol s str
        in L (SrcSpanAnn  { ann = EpAnnNotUsed
                         , locA = RealSrcSpan
                                    (mkRealSrcSpan (mkRealSrcLoc "test/files/Basic.hs" ar ac)
                                                   (mkRealSrcLoc "b" br bc))
                                    S.Nothing
             })
             (mkModuleNameFS (mkFastStringByteList (B.unpack bs)))

psLHsDecl :: Parser (LHsDecl GhcPs)
psLHsDecl = withSpan psTypeSig buildHsDecl
  where
    buildHsDecl :: Sig GhcPs -> Span -> Parser (LHsDecl GhcPs)
    buildHsDecl x s = do
        str <- ask
        pure $ L (srcSpanAnnListItem s str) (SigD NoExtField x)

psLIdP :: Parser (LIdP GhcPs)
psLIdP = withSpan varOcc' buildLIdP
  where
    buildLIdP :: RdrName -> Span -> Parser (LIdP GhcPs)
    buildLIdP x s = do
        str <- ask
        pure $ L (srcSpanEpAnnNotUsed s str) x

psTyVar :: Parser (LHsType GhcPs)
psTyVar = withSpan psLIdP buildTyVar
  where
    buildTyVar :: LIdP GhcPs -> Span -> Parser (LHsType GhcPs)
    buildTyVar x s = do
        str <- ask
        pure $ L (srcSpanEpAnnNotUsed s str) (HsTyVar (tyVarAnn s str) NotPromoted x)

    tyVarAnn s str = EpAnn (Anchor (rlSrcSpan s str) UnchangedAnchor)
                           []
                           (EpaComments [])

psHsWC :: Parser (LHsSigWcType GhcPs)
psHsWC = withSpan psHsSig buildHsWC
  where
    buildHsWC :: HsSigType GhcPs -> Span -> Parser (LHsSigWcType GhcPs)
    buildHsWC x s = do
        str <- ask
        pure $ HsWC NoExtField (L (srcSpanEpAnnNotUsed s str) x)

psTypeSig :: Parser (Sig GhcPs)
psTypeSig = do
    x <- withSpan' psLIdP
    ws
    (_, s') <- withSpan' $(symbol' "::")
    ws
    hswc <- withSpan' $ psHsWC `cut'` Msg "HsWC"
    buildTypeSig x s' hswc
  where
    buildTypeSig :: (LIdP GhcPs, Span) -> Span -> (LHsSigWcType GhcPs, Span) -> Parser (Sig GhcPs)
    buildTypeSig (x, s) s' (hswc, _) = do
        str <- ask
        pure $ TypeSig (sigEpAnn s s' str) [x] hswc

    sigEpAnn s s' str = EpAnn (Anchor (rlSrcSpan s str) UnchangedAnchor)
                       (AnnSig (AddEpAnn AnnDcolon (EpaSpan (rlSrcSpan s' str))) [])
                       (EpaComments [])

psHsSig :: Parser (HsSigType GhcPs)
psHsSig = withSpan psTyApp buildHsSig
  where
    buildHsSig :: HsType GhcPs -> Span -> Parser (HsSigType GhcPs)
    buildHsSig x s = do
        str <- ask
        pure $ HsSig NoExtField (HsOuterImplicit NoExtField) (L (srcSpanEpAnnNotUsed s str) x)


psTupleTy :: Parser (LHsType GhcPs)
psTupleTy = do
    (_, s) <- withSpan' $(symbol "(")
    ws
    (_, s') <- withSpan' $(symbol ")")
    buildTupleTy [] s s'
  where
    buildTupleTy :: [LHsType GhcPs] -> Span -> Span -> Parser (LHsType GhcPs)
    buildTupleTy x s@(Span p _) s'@(Span _ p') = do
        str <- ask
        pure $ L (srcSpanEpAnnNotUsed (Span p p') str) (HsTupleTy (tupleTyAnn s s' str) HsBoxedOrConstraintTuple x)

    tupleTyAnn s s' str = EpAnn (Anchor (rlSrcSpan s str) UnchangedAnchor)
                                (AnnParen AnnParens (EpaSpan (rlSrcSpan s str)) (EpaSpan (rlSrcSpan s' str)))
                                (EpaComments [])

psTyApp :: Parser (HsType GhcPs)
psTyApp = do
    x <- psTyVar `cut'` Msg "IO"
    ws
    y <- psTupleTy `cut'` Msg "()"
    pure $ HsAppTy NoExtField x y

---
---
---
---

fDecl2 :: Parser (LHsDecl GhcPs)
fDecl2 = do
    idp <- psLIdP <* ws
    $(symbol "=") <* ws
    $(symbol "do") <* ws
    rhs <- psGRHSs
    mkHsDecl idp rhs
  where
    mkHsDecl :: LIdP GhcPs -> GRHSs GhcPs (LocatedA (HsExpr GhcPs)) -> Parser (LHsDecl GhcPs)
    mkHsDecl idp rhs = do
      str <- ask
      let s = Span (Pos 1) (Pos 10)
      pure $ L
        (srcSpanAnnListItem s str)
        (ValD NoExtField (FunBind NoExtField
                                  idp
                                  (MG NoExtField
                                      (L (srcSpanEpAnnNotUsed s str) [L (srcSpanEpAnnNotUsed s str) (Match (matchAnn s str)
                                                              (FunRhs idp Prefix NoSrcStrict)
                                                              []
                                                              rhs)])
                                      FromSource)
                                  []
        ))

    psGRHSs :: Parser (GRHSs GhcPs (LocatedA (HsExpr GhcPs)))
    psGRHSs = withSpan psHsExpr buildGRHSs
      where
        buildGRHSs :: HsExpr GhcPs -> Span -> Parser (GRHSs GhcPs (LocatedA (HsExpr GhcPs)))
        buildGRHSs x s = do
            str <- ask
            pure $ GRHSs (EpaComments []) [L (srcSpanEpAnnNotUsed s str) (GRHS (grhsAnn s str) [] (L (srcSpanEpAnnNotUsed s str) x))] (EmptyLocalBinds NoExtField)

    psHsExpr :: Parser (HsExpr GhcPs)
    psHsExpr = withSpan psDo buildHsDo
      where
        buildHsDo :: XRec GhcPs [ExprLStmt GhcPs] -> Span -> Parser (HsExpr GhcPs)
        buildHsDo x s = do
            str <- ask
            pure $ HsDo (srcSpanAnnDo s str) (DoExpr Nothing) x

    psDo :: Parser (XRec GhcPs [ExprLStmt GhcPs])
    psDo = withSpan psStmtLR buildExpr
      where
        buildExpr :: ExprLStmt GhcPs -> Span -> Parser (XRec GhcPs [ExprLStmt GhcPs])
        buildExpr x s = do
            str <- ask
            pure $ L (SrcSpanAnn (doExprAnn s str) (RealSrcSpan (rlSrcSpan s str) S.Nothing)) [x]

    psStmtLR :: Parser (ExprLStmt GhcPs)
    psStmtLR = withSpan psStmt buildExpr
      where
        buildExpr :: StmtLR GhcPs GhcPs (LocatedA (HsExpr GhcPs)) -> Span -> Parser (ExprLStmt GhcPs)
        buildExpr x s = do
            str <- ask
            pure $ L (srcSpanEpAnnNotUsed s str) x

    psStmt :: Parser (StmtLR GhcPs GhcPs (LocatedA (HsExpr GhcPs)))
    psStmt = withSpan psApp buildExpr
      where
        buildExpr x s = do
            str <- ask
            pure $ BodyStmt NoExtField
                            (L (srcSpanEpAnnNotUsed s str) x)
                            NoExtField
                            NoExtField
    
    psApp :: Parser (HsExpr GhcPs)
    psApp = do
        x <- withSpan' psHsVar <* ws
        y <- withSpan' psHsLit
        buildExpr x y
      where
        buildExpr (x, s@(Span a _)) (y, s'@(Span _ b)) = do
            str <- ask
            pure $ HsApp (noEpAnn (Span a b) str)
                         (L (srcSpanEpAnnNotUsed s str) x)
                         (L (srcSpanEpAnnNotUsed s' str) y)

    psHsVar :: Parser (HsExpr GhcPs)
    psHsVar = withSpan varOcc' buildExpr
      where
        buildExpr :: RdrName -> Span -> Parser (HsExpr GhcPs)
        buildExpr x s = do
            str <- ask
            pure $ HsVar NoExtField (L (srcSpanEpAnnNotUsed s str) x)

    psHsLit :: Parser (HsExpr GhcPs)
    psHsLit = withSpan litString buildExpr
      where
        buildExpr :: HsLit GhcPs -> Span -> Parser (HsExpr GhcPs)
        buildExpr x s = do
          str <- ask
          pure $ HsLit (noEpAnn s str) x

-- | Parse an identifier. This parser uses `isKeyword` to check that an identifier is not a
--   keyword.
ident :: Parser B.ByteString
ident = byteStringOf $
  withSpan (identStartChar *> skipMany identChar) (\_ spn -> fails (isKeyword spn))

-- | Parse an identifier, throw a precise error on failure.
ident' :: Parser B.ByteString
ident' = ident `cut'` Msg "identifier"

varid :: Parser B.ByteString
varid = byteStringOf $
    withSpan (identStartChar *> skipMany identChar) (\_ spn -> fails (isKeyword spn))

-- | Parse an identifier, throw a precise error on failure.
tcOcc' :: Parser RdrName
tcOcc' = do
    x <- varid `cut'` Msg "tcOcc"
    pure $ mkRdrUnqual (mkTcOccFS (mkFastStringByteList (B.unpack x)))

varOcc' :: Parser RdrName
varOcc' = do
    x <- varid `cut'` Msg "varOcc"
    pure $ mkRdrUnqual (mkVarOccFS (mkFastStringByteList (B.unpack x)))

litString :: Parser (HsLit GhcPs)
litString = do
  x <- byteStringOf str'
  let x' = B.drop 1 (B.dropEnd 1 x)
  pure $ HsString (SourceText ("\"" ++ utf8ToStr x' ++ "\"")) (mkFastStringByteList (B.unpack x'))

withSpan' :: Parser a -> Parser (a, Span)
withSpan' p = withSpan p (curry return)

-- Convert from 0 indexed to 1 indexed rows and cols
spanToRowCol :: Span -> B.ByteString -> ((Int, Int), (Int, Int))
spanToRowCol (Span a b) str = let [(ar, ac), (br, bc)] = posLineCols str [a, b]
                                  in ((ar + 1, ac + 1), (br + 1, bc + 1))

srcSpanAnnListItem :: Span -> B.ByteString -> SrcSpanAnn' (EpAnn AnnListItem)
srcSpanAnnListItem s str = SrcSpanAnn
    { ann = EpAnn (Anchor (rlSrcSpan s str) UnchangedAnchor)
                  (AnnListItem [])
                  (EpaComments [])
    , locA = RealSrcSpan (rlSrcSpan s str) S.Nothing
    }

srcSpanEpAnnNotUsed :: Span -> B.ByteString -> SrcSpanAnn' (EpAnn ann)
srcSpanEpAnnNotUsed s str = SrcSpanAnn
    { ann = EpAnnNotUsed
    , locA = RealSrcSpan (rlSrcSpan s str) S.Nothing
    }

srcSpanAnnDo :: Span -> B.ByteString -> XDo GhcPs
srcSpanAnnDo s str = EpAnn (Anchor (rlSrcSpan s str) UnchangedAnchor)
                     (AnnList (Just (Anchor (rlSrcSpan s str) UnchangedAnchor)) Nothing Nothing [AddEpAnn AnnDo (EpaSpan (rlSrcSpan s str))] [])
                     (EpaComments [])

rlSrcSpan :: Span -> B.ByteString -> RealSrcSpan
rlSrcSpan s str = let ((ar, ac), (br, bc)) = spanToRowCol s str
                   in mkRealSrcSpan (mkRealSrcLoc "test/files/Basic.hs" ar ac)
                                    (mkRealSrcLoc "b" br bc)


noEpAnn :: Span -> B.ByteString -> EpAnn NoEpAnns
noEpAnn s str = EpAnn (Anchor (rlSrcSpan s str) UnchangedAnchor)
                      NoEpAnns
                      (EpaComments [])

doExprAnn :: Span -> B.ByteString -> EpAnn AnnList
doExprAnn s str = EpAnn (Anchor (rlSrcSpan s str) UnchangedAnchor)
                        (AnnList (Just (Anchor (rlSrcSpan s str) UnchangedAnchor)) Nothing Nothing [] [])
                        (EpaComments [])


matchAnn s str = EpAnn (Anchor (rlSrcSpan s str) UnchangedAnchor)
                 []
                 (EpaComments [])
grhsAnn s str = EpAnn (Anchor (rlSrcSpan s str) UnchangedAnchor)
                 (GrhsAnn Nothing (AddEpAnn AnnEqual (EpaSpan (rlSrcSpan s str))))
                 (EpaComments [])