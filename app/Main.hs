module Main where

import Lexer
import Show

import Language.Haskell.HLint
import GHC.Hs
import GHC.Hs.Dump
import GHC.Types.SrcLoc
import GHC.Utils.Outputable (defaultSDocContext, ppr, printSDocLn, sdocPprDebug)
import GHC.Utils.Ppr
import System.Environment
import System.IO

-- Flatparse
import FlatParse.Basic hiding (Parser, runParser, string, char, cut)
import qualified FlatParse.Basic as FP
import qualified Data.ByteString as B
import GHC.Data.FastString
import qualified GHC.Data.Strict as S
import GHC.Unit.Module.Name
import GHC.Types.Basic
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader

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
--printModule x = printSDocLn sdocCtx (PageMode True) stdout $ ppr x
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
    case FP.runParser fMod str of
        Err _  -> do
            putStrLn "err"
            return Nothing
        OK a _ -> return $ Just a
        Fail   -> do
            putStrLn "uncaught parse error"
            return Nothing

fMod :: Parser (Located HsModule)
fMod = do
    $(keyword "module")
    x <- ident'
    $(keyword "where")
    d1 <- fDecl
    d2 <- fDecl2
    return $ mkGeneralLocated "1:1" (hsMod { hsmodName = Just (mkModName x)
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
    mkModName bs = L
        (SrcSpanAnn  { ann = EpAnnNotUsed
                     , locA = RealSrcSpan
                                (mkRealSrcSpan (mkRealSrcLoc "test/files/Basic.hs" 1 8)
                                               (mkRealSrcLoc "b" 1 12))
                                S.Nothing
        })
        (mkModuleNameFS (mkFastStringByteList (B.unpack bs)))

fDecl :: Parser (LHsDecl GhcPs)
fDecl = do
    return $ mkHsDecl
  where
    spn =
        (SrcSpanAnn  { ann = EpAnn (Anchor rlSrcSpn UnchangedAnchor)
                                   (AnnListItem [])
                                   (EpaComments [])
                     , locA = RealSrcSpan rlSrcSpn S.Nothing
        })
    rlSrcSpn = mkRealSrcSpan (mkRealSrcLoc "test/files/Basic.hs" 3 1)
                             (mkRealSrcLoc "b" 3 14)
    dSpn = SrcSpanAnn
        { ann = EpAnnNotUsed
        , locA = RealSrcSpan dSrcSpn S.Nothing
        }
    dSrcSpn = mkRealSrcSpan (mkRealSrcLoc "test/files/Basic.hs" 3 9)
                            (mkRealSrcLoc "b" 3 14)

    mkHsDecl :: LHsDecl GhcPs
    mkHsDecl = L
        spn
        (SigD NoExtField (TypeSig EpAnnNotUsed
                                  [L dSpn (mkRdrUnqual (mkVarOcc "main"))]
                                  (HsWC NoExtField (L dSpn (HsSig NoExtField
                                                                  (HsOuterImplicit NoExtField)
                                                                  (L dSpn mkTyApp))))
        ))
    mkTyApp :: HsType GhcPs
    mkTyApp = HsAppTy NoExtField
                      (L dSpn (HsTyVar EpAnnNotUsed NotPromoted (L dSpn (mkRdrUnqual (mkTcOcc "IO")))))
                      (L dSpn (HsTupleTy EpAnnNotUsed HsBoxedOrConstraintTuple []))

fDecl2 :: Parser (LHsDecl GhcPs)
fDecl2 = do
    return $ mkHsDecl
  where
    spn =
        (SrcSpanAnn  { ann = EpAnn (Anchor rlSrcSpn UnchangedAnchor)
                                   (AnnListItem [])
                                   (EpaComments [])
                     , locA = RealSrcSpan rlSrcSpn S.Nothing
        })
    rlSrcSpn = mkRealSrcSpan (mkRealSrcLoc "test/files/Basic.hs" 3 1)
                             (mkRealSrcLoc "b" 3 14)
    dSpn = SrcSpanAnn
        { ann = EpAnnNotUsed
        , locA = RealSrcSpan dSrcSpn S.Nothing
        }
    dSrcSpn = mkRealSrcSpan (mkRealSrcLoc "test/files/Basic.hs" 3 9)
                            (mkRealSrcLoc "b" 3 14)

    nSpn = SrcSpanAnn
        { ann = EpAnnNotUsed
        , locA = RealSrcSpan dSrcSpn S.Nothing
        }

    mkHsDecl :: LHsDecl GhcPs
    mkHsDecl = L
        spn
        (ValD NoExtField (FunBind NoExtField
                                  (L dSpn (mkRdrUnqual (mkVarOcc "main")))
                                  (MG NoExtField
                                      (L dSpn [(L dSpn (Match EpAnnNotUsed
                                                              (StmtCtxt (HsDoStmt (DoExpr Nothing)))
                                                              []
                                                              (GRHSs (EpaComments []) [L nSpn (GRHS EpAnnNotUsed [] (L dSpn (HsDo EpAnnNotUsed (DoExpr Nothing) (L dSpn []))))] (EmptyLocalBinds NoExtField))))])
                                      FromSource)
                                  []
        ))

-- | Parse an identifier. This parser uses `isKeyword` to check that an identifier is not a
--   keyword.
ident :: Parser B.ByteString
ident = token $ byteStringOf $
  withSpan (identStartChar *> skipMany identChar) (\_ spn -> fails (isKeyword spn))

-- | Parse an identifier, throw a precise error on failure.
ident' :: Parser B.ByteString
ident' = ident `cut'` (Msg "identifier")
