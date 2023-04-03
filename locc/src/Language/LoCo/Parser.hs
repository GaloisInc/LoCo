{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.LoCo.Parser
  ( Parser,
    Thunk,
    force,
    manyT,
    onSubRegion,
    parseU8,
    runParser,
    topRegion,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Word
import Language.LoCo.Region
import Language.LoCo.Thunk hiding (Thunk, force)
import Language.LoCo.Thunk qualified as Thunk
import Text.Read (readMaybe)

type Error = String

type Document = String

newtype Parser a = Parser
  { unParser :: ExceptT Error (ReaderT Document IO) a
  }
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadError Error,
      MonadReader Document,
      MonadFix,
      MonadIO
    )

type Thunk a = Thunk.Thunk Parser a

class Force a where
  type Result a
  force :: a -> Result a

instance Force (Thunk a) where
  type Result (Thunk a) = Parser a
  force = Thunk.force

instance Force [Thunk a] where
  type Result [Thunk a] = Parser [a]
  force = mapM Thunk.force

instance Force LV' where
  type Result LV' = Parser LV
  force LV' {..} =
    do
      l <- force l'
      v <- force v'
      pure LV {..}

runParser :: Parser a -> Document -> IO (Either Error a)
runParser (Parser p) = runReaderT (runExceptT p)

io :: IO a -> Parser a
io = Parser . lift . lift

topRegion :: Parser Region
topRegion =
  do
    l <- asks length
    pure (0, l)

onSubRegion :: Region -> (Region -> Parser a) -> Parser a
onSubRegion r f = local (take (width r) . drop (begin r)) (f r')
  where
    r' = (0, end r - begin r)

parseU8 :: Region -> Parser (Thunk Word8)
parseU8 region =
  do
    when (width region < 1) $ throwError ""
    input <- ask
    thunkM $
      do
        io (putStrLn $ "parsing from " <> input)
        case readMaybe input of
          Nothing -> throwError ""
          Just r -> pure r

many' :: Int -> (Region -> Parser a) -> Region -> Parser [a]
many' howMany parse region
  | howMany == 0 = pure []
  | width region `mod` howMany /= 0 = throwError ""
  | otherwise =
      do
        pre <- liftEither $ rTake subRegionWidth region
        post <- liftEither $ rDrop subRegionWidth region
        result <- onSubRegion pre parse
        (result :) <$> many (howMany - 1) parse post
  where
    subRegionWidth = width region `div` howMany

many :: Integral l => l -> (Region -> Parser a) -> Region -> Parser [a]
many = many' . fromIntegral

manyT' :: Int -> (Region -> Parser (Thunk a)) -> Region -> Parser [Thunk a]
manyT' howMany parse region
  | howMany == 0 = pure []
  | width region `mod` howMany /= 0 = undefined
  | otherwise =
      do
        pre <- liftEither $ rTake subRegionWidth region
        post <- liftEither $ rDrop subRegionWidth region
        x <- onSubRegion pre parse
        (x :) <$> manyT' (howMany - 1) parse post
  where
    subRegionWidth = width region `div` howMany

manyT :: (Integral n) => Thunk n -> (Region -> Parser (Thunk a)) -> Region -> Parser [Thunk a]
manyT howMany parse region =
  do
    howMany' <- fromIntegral <$> force howMany
    manyT' howMany' parse region

-------------------------------------------------------------------------------

data LV = LV {l :: Word8, v :: [Word8]}

data LV' = LV' {l' :: Thunk Word8, v' :: [Thunk Word8]}

parseLV :: Region -> Parser LV'
parseLV region =
  do
    rl <- rTake 1 region
    l' <- onSubRegion rl parseU8
    rv <- rDrop 1 region
    v' <- onSubRegion rv (manyT l' parseU8)

    pure LV' {..}

fifthElement :: Parser Word8
fifthElement =
  do
    region <- topRegion
    lv' <- parseLV region
    force (((!! 4) . v') lv')

-------------------------------------------------------------------------------

-- constrain [Fn "parseLV", Field "v", Idx 4]
-- lv' <- parseLV region
-- lv'_v <- extractField lv' v'
-- lv'_v_4 <- extractElem lv'_v 4
-- force lv'_v_4

data Accessor = Fn String | Field String | Idx Int

constrain :: [Accessor] -> Parser ()
constrain = undefined

extractField :: a -> (a -> b) -> Parser b
extractField = undefined

extractElem :: [a] -> Int -> Parser a
extractElem = undefined

-- do
--   (results, used) <- unzip <$> go howMany region
--   pure (results, sum used)
-- where
--   go i r

-- many :: Integral n => n -> (Region -> Parser (a, Int)) -> Region -> Parser ([a], Int)
-- many howMany parse region =
--   do
--     (results, used) <- unzip <$> go howMany region
--     pure (results, sum used)
--   where
--     go i r
--       | i == 0 = pure []
--       | otherwise =
--           do
--             (result, used) <- parse r
--             r' <- rtake used r
--             ((result, used) :) <$> go (i - 1) r'

-- parseU8 :: Region -> Parser (Word8, Int)
-- parseU8 region =
--   do
--     when (width region >= 1) $ throwError "too narrow"
--     input <- ask
--     case readMaybe input of
--       Nothing -> throwError (printf "couldn't parse %s" input)
--       Just r -> pure (r, 1)
