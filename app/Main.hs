module Main where

import Language.Haskell.HLint
import GHC.Hs
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import GHC.Utils.Ppr
import System.IO

main :: IO ()
main = do
    result <- useHlint "test/files/Parser.hs"
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
    x <- parseModuleEx' fileToModule flags file Nothing
    case x of
        Right m -> return $ Just $ ghcModule m
        Left _ -> return Nothing

----------------------------------------
-- 
