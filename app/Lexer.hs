module Lexer where

-- Flatparse
import FlatParse.Stateful hiding (Parser, runParser, string, cut)
import qualified FlatParse.Stateful as FP
import qualified Data.ByteString as B
import Data.String
import "template-haskell" Language.Haskell.TH

type Parser = FP.Parser State Error

type State = B.ByteString

-- | An expected item which is displayed in error messages.
data Expected
  = Msg String  -- ^ An error message.
  | Lit String  -- ^ A literal expected thing.
  deriving (Eq, Show, Ord)

instance IsString Expected where fromString = Lit

-- | A parsing error.
data Error
  = Precise Pos Expected     -- ^ A precisely known error, like leaving out "in" from "let".
  | Imprecise Pos [Expected] -- ^ An imprecise error, when we expect a number of different things,
                             --   but parse something else.
  deriving Show

errorPos :: Error -> Pos
errorPos (Precise p _)   = p
errorPos (Imprecise p _) = p

-- | Merge two errors. Inner errors (which were thrown at points with more consumed inputs)
--   are preferred. If errors are thrown at identical input positions, we prefer precise errors
--   to imprecise ones.
--
--   The point of prioritizing inner and precise errors is to suppress the deluge of "expected"
--   items, and instead try to point to a concrete issue to fix.
merge :: Error -> Error -> Error
merge e e' = case (errorPos e, errorPos e') of
  (p, p') | p < p' -> e'
  (p, p') | p > p' -> e
  (p, p')          -> case (e, e') of
    (Precise{}      , _               ) -> e
    (_              , Precise{}       ) -> e'
    (Imprecise _ es , Imprecise _ es' ) -> Imprecise p (es ++ es')
{-# noinline merge #-} -- merge is "cold" code, so we shouldn't inline it.

-- | Imprecise cut: we slap a list of items on inner errors.
cut :: Parser a -> [Expected] -> Parser a
cut p es = do
  pos <- getPos
  FP.cutting p (Imprecise pos es) merge

-- | Precise cut: we propagate at most a single error.
cut' :: Parser a -> Expected -> Parser a
cut' p e = do
  pos <- getPos
  FP.cutting p (Precise pos e) merge

-- | Parse a line comment.
lineComment :: Parser ()
lineComment =
  withOption anyWord8
    (\case 10 -> ws
           _  -> lineComment)
    (pure ())

-- | Parse a potentially nested multiline comment.
multilineComment :: Parser ()
multilineComment = go (1 :: Int) where
  go 0 = ws
  go n = $(switch [| case _ of
    "-}" -> go (n - 1)
    "{-" -> go (n + 1)
    _    -> branch anyWord8 (go n) (pure ()) |])

-- | Consume whitespace.
ws :: Parser ()
ws = $(switch [| case _ of
  " "  -> ws
  "\n" -> ws
  "\t" -> ws
  "\r" -> ws
  "--" -> lineComment
  "{-" -> multilineComment
  _    -> pure () |])

str' :: Parser ()
str' = do
    $(char '"')
    go
  where
    go = $(switch [| case _ of
      "\\\"" -> go
      "\""   -> pure ()
      _      -> branch anyWord8 go (pure ()) |])

-- | Consume whitespace after running a parser.
token :: Parser a -> Parser a
token p = p <* ws
{-# inline token #-}

-- | Read a starting character of an identifier.
identStartChar :: Parser Char
identStartChar = satisfyAscii isLatinLetter
{-# inline identStartChar #-}

-- | Read a non-starting character of an identifier.
identChar :: Parser Char
identChar = satisfyAscii (\c -> isLatinLetter c || isDigit c)
{-# inline identChar #-}

-- | Check whether a `Span` contains exactly a keyword. Does not change parsing state.
isKeyword :: Span -> Parser ()
isKeyword span = inSpan span do
  $(FP.switch [| case _ of
      "let"   -> pure ()
      "in"    -> pure ()
      "if"    -> pure ()
      "then"  -> pure ()
      "else"  -> pure ()
      "true"  -> pure ()
      "false" -> pure ()  |])
  eof

-- | Parse a non-keyword string.
symbol :: String -> Q Exp
symbol str = [| $(FP.string str) |]

-- | Parser a non-keyword string, throw precise error on failure.
symbol' :: String -> Q Exp
symbol' str = [| $(symbol str) `cut'` Lit str |]

-- | Parse a keyword string.
keyword :: String -> Q Exp
keyword str = [| $(FP.string str) `notFollowedBy` identChar |]

-- | Parse a keyword string, throw precise error on failure.
keyword' :: String -> Q Exp
keyword' str = [| $(keyword str) `cut'` Lit str |]
