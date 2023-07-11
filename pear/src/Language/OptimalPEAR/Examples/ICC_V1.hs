{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Language.OptimalPEAR.Examples.ICC_V1 where

-- base pkgs:
import           Control.Monad
import           Data.Word
import qualified Control.Monad.Trans.Except as E

-- local modules:
-- import           Language.PEAR.Examples.RegionParser.Tests(pTwoWords_FxdWd_NoFlT)
import           Language.PEAR.Primitives
import           Language.PEAR.ParserLibrary
import           Language.PEAR.Region.API (Region,r_width)
import qualified Language.PEAR.Region.API as R
import           Language.PEAR.Types
import           Language.PEAR.Util


-- Old NOTEs
--   - a conversion of **.ICCV2 into MEP form.
--     - see **.ICCV2 for notes/TODOs/improvements!
--     - hmmm: import parts of that here?
--     - you are capturing exceptions with PrimFail, not with FailT monad.


---- temporary glue ------------------------------------------------
-- FIXME: *
-- TODO
--  - remove newtypes: easier to use Exception/_ monads
--  - remove all this glue

{- NOTE
data Func m v
  = F_FuncPure ([v] -> v)
  | F_FuncFail ([v] -> Possibly v)
  | F_PrimNoFl ([v] -> m v)
  | F_PrimFail ([v] -> m (Possibly v))
  	-- Defined at LoCo/MEP/Compile.hs:22:1
-}

data RHS m v =
  E{ uses :: [Symbol]
   , func :: [v] -> m v
   }

f_FuncPure :: Monad m => ([v] -> v         )           -> [v] -> FailT m v
f_FuncFail :: Monad m => ([v] -> Possibly v)           -> [v] -> FailT m v
f_PrimNoFl :: Monad m => ([v] -> m v       )           -> [v] -> m v
f_PrimFail :: Monad m => ([v] -> FailT m (Possibly v)) -> [v] -> FailT m v

-- FIXME: abstract: ?

except' :: Monad m => Possibly a -> FailT m a
except' e = FailT $ E.except e

f_FuncPure f vs = return $ f vs

f_FuncFail f vs = except' (f vs)
                    
f_PrimNoFl f = f

f_PrimFail f vs = do
                  v <- f vs
                  except' v

-- old:
-- app_FxdWd_NoFlT ::
--     Monad m => RgnPrsr_FxdWd_NoFlT m a -> Region -> NoFlT m a
app :: Monad m => RgnPrsr_FxdWd_NoFlT m a -> Region -> FailT m a
app p r = lift_NoFlT $ app_FxdWd_NoFlT p r 
  
---- Example: ICC ---------------------------------------------------------

data Val = VR Region
         | VW Word32
         | VB Bool
         | VL [Val]
         | VRS  [Region]   -- redundant
         -- ad hoc constructors
         | VCRS R.CanonicalRegions
         | VTBL [(Word32,Word32)]
         | VTEDS [TED]
         deriving (Eq, Ord, Read, Show)

icc :: (Monad m) => Region -> Env (RHS (FailT m) Val)
icc r0 =
  let
    wCnt = width_FxdWd pWord32_FxdWd_NoFl
  in 
  [ "R_CNT,R2" -->
         E [] $ f_FuncFail $
         \[] -> 
         do
         (r1,r2) <- {- E.except $ -}
                    R.split1_Possibly r0 wCnt
         return $ VRS [r1,r2]

  , "cnt" -->
        E ["R_CNT,R2"] $ f_PrimNoFl $
        \[VRS [r_cnt,_]]-> 
        VW <$> app pWord32_FxdWd_NoFlT r_cnt

  , "tbl,R3" -->
        E ["R_CNT,R2","cnt"] $ f_PrimFail $
        \[VRS [_r_cnt,r2], VW cnt]-> 
        do
        let lpTbl = pMany_FxdWd_NoFlT (fromIntegral cnt) pTwoWords_FxdWd_NoFlT
                    -- NOTE, a _NoFl parser (not applied to region yet)
        case R.split1_Possibly r2 (width_FxdWd lpTbl) of
          Right (r_tbl, r3) ->
              do
              tbl  <- lpTbl `app` r_tbl 
              return $ Right $ VL [VTBL tbl, VR r3]
          Left ms ->
              return $ Left ms
              
  , "tbl" -->
        E ["tbl,R3"] $ f_FuncPure $
        \[VL[tbl,_r3]]->
          tbl
        
  , "R3" -->
        E ["tbl,R3"] $ f_FuncPure $
        \[VL[_tbl,r3]]->
          r3
        
  , "R_TEDS" -->
        E ["tbl"] $ f_FuncFail $
        \[VTBL tbl]-> 
          VRS <$>
          (forM tbl $
            \(loc,sz)-> R.subRegion_Possibly r0 (toLoc loc) (toLoc sz))
  
   , "teds" -->
        E ["R_TEDS"] $ f_PrimNoFl $
        \[VRS r_teds]-> 
           do
           teds <- forM r_teds $
                     \r-> app (pTED_FxdWd_NoFlT (r_width r)) r
           return $ VTEDS teds
           
  , "RS_MERGEDDISJ" -->
        E ["R_CNT,R2","R3","R_TEDS"] $ f_FuncFail $
        \[VRS[r_cnt,r2], VR r3, VRS r_teds] -> 
        do
        let r_tbl = r2 `R.regionMinusSuffix` r3
        crs <- elaboratePossibly ["TED values are not disjoint:"]
             $ R.regionsDisjoint_Possibly (r_cnt : r_tbl : r_teds)
        return $ VCRS crs
        
  , "CAVITYFREE" -->
        E ["RS_MERGEDDISJ"] $ f_FuncFail $
        \[VCRS crs] -> 
        do
        let cavities = r0 `R.complementCRs` crs
        case cavities of
          R.CR [] -> Right $ VB True
          R.CR rs -> Left  $ ["Cavities present:"
                             , ""
                             ] ++ map ((" "++) . R.ppRegion) rs

  , "teds_safe" -->
        E ["teds", "CAVITYFREE"] $ f_FuncPure $
        \[teds,VB True]->
        teds
        -- note: cavityFree must have succeeded.
  ]

  
---- primitives --------------------------------------------------------------

pTED_FxdWd_Fail :: Width -> RgnPrsr_FxdWd_Fail TED
pTED_FxdWd_Fail w = lift_FxdWd_NoFl $ mkPrim_FxdWd_NoFl w TED

-- this parser cannot fail (...)
pTED_FxdWd_NoFl :: Width -> RgnPrsr_FxdWd_NoFl TED
pTED_FxdWd_NoFl w = mkPrim_FxdWd_NoFl w TED

pTED_FxdWd_NoFlT :: Monad m => Width -> RgnPrsr_FxdWd_NoFlT m TED
pTED_FxdWd_NoFlT w = mkPrim_FxdWd_NoFlT w TED


-- a TED [for now] is just the string pointed to!
newtype TED = TED String
              deriving (Eq,Ord,Read,Show)

