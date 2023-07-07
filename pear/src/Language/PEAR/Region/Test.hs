module Language.PEAR.Region.Test where  

-- base pkgs:
import Data.Either
import Data.Function
import Data.List

-- local modules:
import Language.PEAR.Region.Implem
import Language.PEAR.Util

---- shortcuts ---------------------------------------------------------------

rdp :: [Region] -> Possibly CanonicalRegions
rdp = regionsDisjoint_Possibly

ar :: Region -> CanonicalRegions -> Possibly CanonicalRegions
ar  = addNonOverlappingRegion

gc :: Region -> Region -> (Region, Region)
gc  = getContext


---- properties --------------------------------------------------------------

prop_testRegions = and props

props = [ and tests1
        , and tests2
        , and tests3
        , and tests4 
        ]

---- test [inputs] -----------------------------------------------------------

tests1 = [ R 0 1 `regionPrecedes` R 1 1
         , R 0 0 `regionPrecedes` R 1 0 
         , R 0 0 `regionPrecedes` R 0 0  -- ?? odd, but leave it
         , not $ R 0 2 `regionPrecedes` R 1 1
         , not $ R 2 0 `regionPrecedes` R 1 0
         , R 0 2 `regionsOverlap` R 1 1     
         , not $ R 0 2 `regionsOverlap` R 2 0  
         , not $ R 0 2 `regionsOverlap` R 2 1  
         , R 0 2 `regionContains` R 0 0
         , R 0 2 `regionContains` R 0 1
         , R 0 2 `regionContains` R 0 2
         , R 0 2 `regionContains` R 1 0  -- note!
         , R 0 2 `regionContains` R 2 0  -- odd!!
         , not $ R 0 2 `regionContains` R 3 0
         , not $ R 0 2 `regionContains` R 2 1
         ]

tests2 = [ R 0 0 `ar` CR []          == Right (CR [])
         , R 0 1 `ar` CR [R 0 1]     & isLeft
         , R 0 1 `ar` CR [R 2 1]     == Right (CR [R 0 1,R 2 1])
         , R 2 1 `ar` CR [R 1 1]     == Right (CR [R 1 2])      
         , R 2 0 `ar` CR [R 1 1]     == Right (CR [R 1 1])      
         , R 3 1 `ar` CR [R 1 2]     == Right (CR [R 1 3])
         ]                                                      

---- tests3: gc --------------------------------------------------------------

testInputs3 = [ (R 0 10, R 2 3)
              , (R 0 10, R 0 3)
              , (R 0 10, R 7 3)
              ] 

tests3 = map getContextVerified testInputs3

getContextVerified (r0,r1) = (rdp [ra,r1,rb] == Right (CR [r0]))
  where
  (ra,rb) = gc r0 r1
  
---- region examples ---------------------------------------------------------

rs_a = splitWidths  (R 0 6) [2,2,2]
rs_b = splitWidths' (R 0 9) [2,2,2]

rs_1 = [ R {r_start = 0, r_width = 20}
       , R {r_start = 21, r_width = 5}
       , R {r_start = 26, r_width = 2}
       ]
       
rs_2 = [ R {r_start = 26, r_width = 2}
       , R {r_start = 21, r_width = 5}
       ]

rss  = permutations rs_1

---- tests4: rdp -------------------------------------------------------------

tests4 = tests4a ++ tests4b

tests4a = [ rdp rs' == Right(CR [R 0 20, R 21 7]) | rs' <- rss]
tests4b = [ rdp rs_2 == Right (CR [R 21 7])
          , rdp rs_a == Right (CR [R 0 6])
          , rdp rs_b == Right (CR [R 0 9])
          ]
  
