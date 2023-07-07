{-# LANGUAGE LambdaCase #-}

module Language.PEAR.Util where

import Data.List
import Data.Word


---- stubs and such ----------------------------------------------------------

data TBD
data STUB

stub = error "stub"


---- types universal to project ----------------------------------------------

-- | file locations (and widths too): unsigned.
type Loc = Word64
type Width = Loc

toLoc :: Integral a => a -> Loc
toLoc = fromIntegral


---- Possibly ----------------------------------------------------------------

type Errors = [String]
type Possibly a = Either Errors a

ppPossibly :: Show a => Possibly a -> String
ppPossibly x = unlines $
                 case x of
                   Left ss -> "Failure:" : map (" "++) ss
                   Right y -> ["Success:", show y]

printPossibly x = putStrLn $ ppPossibly x
  
elaboratePossibly :: [String] -> Possibly b -> Possibly b
elaboratePossibly ss = \case
                         Left ss' -> Left (ss ++ ss')
                         Right x  -> Right x
                
---- Utilities ---------------------------------------------------------------

unsafeLookup e k = flip getEnv e k

-- unsafe version of lookup
getEnv :: (Eq k, Show k) => [(k, a)] -> k -> a
getEnv env v = case lookup v env of
                 Just r  -> r
                 Nothing -> error
                          $ unwords ["getEnv:", show v, show (map fst env)]

---- reads variations --------------------------------------------------------

readM :: Read a => String -> Maybe a
readM s = case reads s of
             [(a,[])] -> Just a
             _        -> Nothing

readsM :: Read a => String -> Maybe (a, String)
readsM s = case reads s of
             [(a,s')] -> Just (a,s')
             _        -> Nothing

readsPWidth :: Read a => String -> Possibly (a, Width)
readsPWidth s =
  case reads s of
    [(a,s')] -> Right (a, genericLength s - genericLength s')
                -- ^ not efficient but ...?!
    _        -> Left ["cannot parse: " ++ s]

