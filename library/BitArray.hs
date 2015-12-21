module BitArray where

import BitArray.Prelude
import qualified BitArray.Parser as Parser
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Data.Foldable as Foldable
import qualified Data.MonoTraversable as MonoTraversable
import qualified NumericQQ


-- |
-- A @newtype@ wrapper which provides an array-like interface to a type,
-- which has instances of 'Bits', 'FiniteBits' and 'Num'.
--
-- You can construct bit arrays by wrapping numeric values:
--
-- >>> BitArray (7 :: Int8)
-- [qq|00000111|]
--
-- or directly from numeric literals:
--
-- >>> 7 :: BitArray Int8
-- [qq|00000111|]
--
-- or using a binary notation quasi-quoter,
-- assuming you have the @QuasiQuotes@ pragma turned on:
--
-- >>> [qq|0111|] :: BitArray Int8
-- [qq|00000111|]
--
-- @BitArray@ derives the 'Bits' and 'FiniteBits' instances from the base type,
-- so it supports all the standard bitwise operations for fixed-size integral
-- types.
newtype BitArray a = BitArray a
  deriving (Bounded, Enum, Eq, Integral, Data, Num, Ord, Real, Ix, Generic,
            Typeable, Bits, FiniteBits)

-- |
-- Produces a literal of zeros and ones.
--
-- >>> show (BitArray (5 :: Int8))
-- "[qq|00000101|]"
instance (FiniteBits a) => Show (BitArray a) where
  show = wrap . toString
    where
      wrap = ("[qq|" ++) . (++ "|]")

-- |
-- Parses a literal of zeros and ones.
--
-- >>> read "[qq|1110|]" :: BitArray Int8
-- [qq|00001110|]
--
-- >>> unwrap (read "[qq|1110|]") :: Int
-- 14
instance (FiniteBits a) => Read (BitArray a) where
  readsPrec = const $ ReadP.readP_to_S $ parser
    where
      parser =
        BitArray <$> ReadP.string "[qq|" *> Parser.bits <* ReadP.string "|]"

instance (FiniteBits a) => IsString (BitArray a) where
  fromString =
    fromMaybe (error "Unparsable bit array string") . parseString

type instance MonoTraversable.Element (BitArray a) = Bool

instance (FiniteBits a) => MonoTraversable.MonoFunctor (BitArray a) where
  omap f t = foldrBits (\b w -> setFirstBit (f b) .|. shiftL w 1) zeroBits t where
    setFirstBit True = setBit zeroBits 0
    setFirstBit _    = zeroBits
  {-# INLINE omap #-}

instance (FiniteBits a) => MonoTraversable.MonoFoldable (BitArray a) where
  ofoldMap f = foldrBits (mappend . f) mempty
  ofoldr = foldrBits
  ofoldl' f z0 t = foldrBits f' id t z0 where
    f' x k z = k $! f z x
  ofoldr1Ex f t = indexedFoldrBits 1 f (testBit t 0) t
  ofoldl1Ex' f t = indexedFoldrBits 1 f' id t (testBit t 0) where
    f' x k z = k $! f z x
  {-# INLINE headEx #-}
  {-# INLINE lastEx #-}
  {-# INLINE maximumByEx #-}
  {-# INLINE minimumByEx #-}
  {-# INLINE oall #-}
  {-# INLINE oany #-}
  {-# INLINE ocompareLength #-}
  {-# INLINE ofoldMap #-}
  {-# INLINE ofoldMap1Ex #-}
  {-# INLINE ofoldl' #-}
  {-# INLINE ofoldl1Ex' #-}
  {-# INLINE ofoldlM #-}
  {-# INLINE ofoldr #-}
  {-# INLINE ofoldr1Ex #-}
  {-# INLINE oforM_ #-}
  {-# INLINE ofor_ #-}
  {-# INLINE olength #-}
  {-# INLINE olength64 #-}
  {-# INLINE omapM_ #-}
  {-# INLINE onull #-}
  {-# INLINE otoList #-}
  {-# INLINE otraverse_ #-}
  {-# INLINE unsafeHead #-}
  {-# INLINE unsafeLast #-}

{-# INLINE foldrBits #-}
foldrBits :: (FiniteBits a)
          => (MonoTraversable.Element (BitArray a) -> b -> b)
          -> b
          -> BitArray a
          -> b
foldrBits = indexedFoldrBits 0

{-# INLINE indexedFoldrBits #-}
indexedFoldrBits :: (FiniteBits a)
                 => Int
                 -> (MonoTraversable.Element (BitArray a) -> b -> b)
                 -> b
                 -> BitArray a
                 -> b
indexedFoldrBits i f z t
  | i == finiteBitSize t = z
  | otherwise = testBit t i `f` indexedFoldrBits (1 + i) f z t

-- * Constructors and converters
-------------------------

-- |
-- A binary number quasi-quoter.
-- Produces a numeric literal at compile time.
-- Can be used to construct both bit arrays and integral numbers.
--
-- >>> [qq|011|] :: Int
-- 3
--
-- >>> [qq|011|] :: BitArray Int8
-- [qq|00000011|]
qq = NumericQQ.bin

-- | Unwrap the underlying value of a bit array.
unwrap :: BitArray a -> a
unwrap (BitArray a) = a

-- ** Strings
-------------------------

-- |
-- Convert into a binary notation string.
--
-- >>> toString (BitArray (5 :: Int8))
-- "00000101"
toString :: (FiniteBits a) => BitArray a -> String
toString = fmap (\case True -> '1'; False -> '0') . reverse . toBoolList

-- |
-- Parse a binary notation string.
--
-- >>> parseString "123" :: Maybe (BitArray Int8)
-- Nothing
--
-- >>> parseString "101" :: Maybe (BitArray Int8)
-- Just [qq|00000101|]
parseString :: (FiniteBits a) => String -> Maybe (BitArray a)
parseString = fmap fst . listToMaybe . ReadP.readP_to_S Parser.bits

-- ** Lists
-------------------------

-- |
-- Convert into a list of set bits.
--
-- The list is ordered from least significant to most significant bit.
{-# INLINABLE toSetBitList #-}
toSetBitList :: (FiniteBits a) => BitArray a -> [a]
toSetBitList (BitArray w) =
  processIndexes [0 .. (pred . finiteBitSize) w]
  where
    processIndexes = filter (\w' -> w .&. w' /= zeroBits) . fmap bit

-- | Construct from a list of set bits.
{-# INLINABLE fromSetBitList #-}
fromSetBitList :: (FiniteBits a) => [a] -> BitArray a
fromSetBitList = BitArray . inline Foldable.foldr (.|.) zeroBits

-- |
-- Convert into a list of boolean values,
-- which represent the \"set\" flags of each bit.
--
-- The list is ordered from least significant to most significant bit.
{-# INLINABLE toBoolList #-}
toBoolList :: (FiniteBits a) => BitArray a -> [Bool]
toBoolList = MonoTraversable.otoList

-- |
-- Construct from a list of boolean flags for the "set" status of each bit.
--
-- The list must be ordered from least significant to most significant bit.
{-# INLINABLE fromBoolList #-}
fromBoolList :: (FiniteBits a) => [Bool] -> BitArray a
fromBoolList = inline fromSetBitList . fmap (bit . fst) . filter snd . zip [zeroBits..]

-- * Utils
-------------------------

-- | Map over the set bits.
{-# INLINABLE mapSetBitList #-}
mapSetBitList :: (FiniteBits a, FiniteBits b) => (a -> b) -> BitArray a -> BitArray b
mapSetBitList f = inline fromSetBitList . fmap f . inline toSetBitList

-- | Perform a right-associative fold over the set bits.
{-# INLINABLE foldrSetBitList #-}
foldrSetBitList :: (FiniteBits a) => (a -> b -> b) -> b -> BitArray a -> b
foldrSetBitList step init = inline Foldable.foldr step init . inline toSetBitList

-- | Traverse thru set bits.
{-# INLINABLE mapSetBitListM_ #-}
mapSetBitListM_ :: (FiniteBits a, Monad m) => (a -> m b) -> BitArray a -> m ()
mapSetBitListM_ f = inline Foldable.mapM_ f . inline toSetBitList

-- | Traverse thru set bits.
{-# INLINABLE traverseSetBitListM_ #-}
traverseSetBitListM_ :: (FiniteBits a, Applicative f) => (a -> f b) -> BitArray a -> f ()
traverseSetBitListM_ f = inline Foldable.traverse_ f . inline toSetBitList
