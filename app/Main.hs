module Main where

import Lexer

import Language.Haskell.HLint
import GHC.Hs
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
printModule x = printSDocLn sdocCtx (PageMode True) stdout $ ppr x
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
    return $ mkGeneralLocated "1:1" (hsMod { hsmodName = Just (mkModName x)
                                           , hsmodDecls = [d1]
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
        (SrcSpanAnn  { ann = EpAnnNotUsed
                     , locA = RealSrcSpan
                                (mkRealSrcSpan (mkRealSrcLoc "test/files/Basic.hs" 1 8)
                                               (mkRealSrcLoc "b" 1 12))
                                S.Nothing
        })
    mkHsDecl = L
        spn
        (SigD NoExtField (TypeSig EpAnnNotUsed [] (HsWC NoExtField (L spn (HsSig NoExtField
                                                                        (HsOuterImplicit NoExtField)
                                                                        (L spn (HsStarTy NoExtField True))
                                                                        )))))

-- | Parse an identifier. This parser uses `isKeyword` to check that an identifier is not a
--   keyword.
ident :: Parser B.ByteString
ident = token $ byteStringOf $
  withSpan (identStartChar *> skipMany identChar) (\_ spn -> fails (isKeyword spn))

-- | Parse an identifier, throw a precise error on failure.
ident' :: Parser B.ByteString
ident' = ident `cut'` (Msg "identifier")
