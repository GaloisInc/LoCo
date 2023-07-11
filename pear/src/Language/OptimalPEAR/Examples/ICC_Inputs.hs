module Language.OptimalPEAR.Examples.ICC_Inputs where

d1 =
  "0000"
  -- FIXME: error in computing cavities

d2 =
  concat 
  ["0001"         -- cnt
  ,"0012","0005"  -- offset,size
  ,"abcde"        -- TED, loc=12
  ]


d3 =
  concat 
  ["0002"         -- cnt
  ,"0020","0005"  -- offset, size
  ,"0025","0003"  -- offset, size
  ,"abcde"        -- TED, loc=20
  ,"fgh"          -- TED, loc=25
  ]
  

d4e = -- error:
  concat
  ["0002"         -- cnt
  ,"0020","0005"  -- offset, size
  ,"0025","0099"  -- offset, size  NOTE error
  ,"abcde"        -- TED, loc=20
  ,"fgh"          -- TED, loc=25
  ]
  -- note: no parsing of any TEDs
  

d5 = -- overlapping regions!
  concat
  ["0002"         -- cnt
  ,"0020","0005"  -- offset, size
  ,"0000","0008"  -- offset, size : suspicious,
                  -- but legal!
  ,"abcde"        -- TED, loc=20
  ,"fgh"          -- TED, loc=25  : dead/unread
  ]


d6 = -- overlapping TED values
  concat
  ["0002"         -- cnt
  ,"0020","0005"  -- offset, size
  ,"0024","0004"  -- offset, size : will overlap!
  ,"abcde"        -- TED, loc=20  : 'e' read 2x
  ,"fgh"          -- TED, loc=25  
  ]
  -- note the error message: due to
  -- joining of regions, less clear than
  -- desirable.

d7 = -- contains cavities:
  concat
  ["0002"         -- cnt
  ,"0021","0005"  -- offset, size
  ,"0026","0002"  -- offset, size
  -- spaces are cavities here:
  ," abcde"       -- TED, loc=21 
  ,"fg    "       -- TED, loc=25  
  ]

  

ds = [d1,d2,d3,d5,d6]
  -- these all good for 'teds' but not 'safe_teds'

---- cavities and such -------------------------------------------------------
-- if we check for cavities,these will be errors:

e1 = d5 -- overlapping regions
e2 = d6
e3 = d7 -- cavities


es = [e1,e2,e3]
