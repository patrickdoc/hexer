{- https://github.com/egolf-cs/Verbatim/blob/main/Verbatim/regex.v -}
module Match (
    Regex(..),
    derivative,
    match
) where

data Regex
  = EmptySet
  | EmptyStr
  | Chr Char
  | App Regex Regex
  | Union Regex Regex
  | Star Regex
  deriving (Show, Eq)

nullable' :: Regex -> Bool
nullable' EmptySet      = False
nullable' EmptyStr      = True
nullable' (Chr _)       = False
nullable' (App r1 r2)   = nullable' r1 && nullable' r2
nullable' (Union r1 r2) = nullable' r1 && nullable' r2
nullable' (Star _)      = True

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

nullable :: Regex -> Bool
nullable EmptySet      = False
nullable EmptyStr      = True
nullable (Chr _)       = False
nullable (App r1 r2)   = if' (not (nullable r2)) False (nullable r1)
nullable (Union r1 r2) = if' (nullable r2) True (nullable r1)
nullable (Star _)      = True

derivative :: Char -> Regex -> Regex
derivative _ EmptySet      = EmptySet
derivative _ EmptyStr      = EmptySet
derivative a (Chr b)       = if' (a == b) EmptyStr EmptySet
derivative a (App r1 r2)   = if' (nullable r1)
                               (Union (App (derivative a r1) r2) (derivative a r2))
                               (App (derivative a r1) r2)
derivative a (Union r1 r2) = Union (derivative a r1) (derivative a r2)
derivative a (Star r)      = App (derivative a r) (Star r)

derivativeList :: String -> Regex -> Regex
derivativeList []     r = r
derivativeList (c:cs) r = derivativeList cs (derivative c r)

iterUnion :: [Regex] -> Regex
iterUnion [] = EmptySet
iterUnion [r] = r
iterUnion (r:rs) = Union r (iterUnion rs)

iterApp :: [Regex] -> Regex
iterApp [] = EmptyStr
iterApp [r] = r
iterApp (r:rs) = App r (iterApp rs)

match :: String -> Regex -> Bool
match []     r = nullable r
match (c:cs) r = match cs (derivative c r)

-- maxPrefOne :: String -> Regex -> Maybe (String, String)
