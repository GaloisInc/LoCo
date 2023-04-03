{-# LANGUAGE FlexibleContexts #-}

module Language.LoCo.Region where

import Control.Monad.Except

type Region = (Int, Int)

begin :: Region -> Int
begin = fst

end :: Region -> Int
end = snd

width :: Region -> Int
width = liftM2 (-) end begin

rTake :: MonadError String m => Int -> Region -> m Region
rTake i r
  | begin r + i > end r = throwError "bad take"
  | otherwise = pure (begin r, begin r + i)

rDrop :: MonadError String m => Int -> Region -> m Region
rDrop i r
  | begin r + i > end r = throwError "bad take"
  | otherwise = pure (begin r + i, end r)
