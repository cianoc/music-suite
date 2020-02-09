{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

-- | Common (Western classical) pitches, intervals and related types.
module Music.Pitch.Common.Types
{- TODO restore this in Common.Types after moving definitions to Common.Internal:
  ( -- * Even octaves and steps
    Octaves,
    DiatonicSteps,
    ChromaticSteps,
    Semitones,

    -- * Intervals
    Number,
    Quality (..),
    QualityType (..),
    IntervalBasis (..),
    Interval (..),

    -- * Pitch
    Name (..),
    Accidental,
    Pitch (..),
  )
-}
where

import Data.AdditiveGroup
-- import Data.VectorSpace
import Data.AffineSpace
import Data.Typeable
-- import Music.Pitch.Literal
import Data.Functor.Couple
import Music.Pitch.Alterable
import Music.Pitch.Augmentable
import Data.VectorSpace
import Data.AffineSpace
import Control.Applicative
import Control.Lens hiding (simple)
import Control.Monad
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson
import Data.AffineSpace.Point (relative)
import Data.Basis
import Data.Either
import qualified Data.List as List
import Data.Maybe
import Data.Semigroup
import Data.Typeable
import Data.VectorSpace
import Numeric.Positive


-- |
-- Number of chromatic steps.
-- May be negative, indicating a downward interval.
newtype ChromaticSteps = ChromaticSteps {getChromaticSteps :: Integer}
  deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

-- |
-- Number of diatonic steps.
-- May be negative, indicating a downward interval.
newtype DiatonicSteps = DiatonicSteps {getDiatonicSteps :: Integer}
  deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

-- |
-- Number of octaves.
-- May be negative, indicating a downward interval.
newtype Octaves = Octaves {getOctaves :: Integer}
  deriving (Eq, Ord, Num, Enum, Real, Integral)

-- |
-- Number of semitones.
-- May be negative, indicating a downward interval.
type Semitones = ChromaticSteps

-- |
-- The /number/ component of an interval (fourth, fifth) etc.
-- May be negative, indicating a downward interval.
newtype Number = Number {getNumber :: Integer}
  deriving (Eq, Ord, Num, Enum, Real, Integral)

-- |
-- The /quality/ component of an interval (minor, major, augmented).
-- Generalized from single\/double augmented\/diminished to any number of steps.
data Quality
  = Augmented Integer
  | Major
  | Perfect
  | Minor
  | Diminished Integer
  deriving (Eq, Ord, Show)

{-
TODO we really want to use Positive instead of Integer
Alternatively, we could do it as a recursive type

data Quality
  = Major
  | Minor
  | Perfect
  | Augment Quality
  | Diminish Quality
-}

-- |
-- The actual alteration implied by a quality is dependent on whether it is attached
-- to a major\/minor vs. a perfect-style number. This type represents the two possibilities.
data QualityType = PerfectType | MajorMinorType
  deriving (Eq, Ord, Read, Show)

-- |
-- Accidental, represented as number of alterations.
-- Generalized from natural and single\/double sharp\/flat to any number of steps.
newtype Accidental = Accidental {getAccidental :: Integer}
  deriving (Eq, Ord, Num, Enum, Real, Integral)

-- |
-- Pitch name component.
data Name = C | D | E | F | G | A | B
  deriving (Eq, Ord, Show, Enum)

-- |
-- This type represents standard basis for intervbals.
data IntervalBasis = Chromatic | Diatonic
  deriving (Eq, Ord, Show, Enum)

-- |
-- Interval type.
newtype Interval = Interval {getInterval :: (ChromaticSteps, DiatonicSteps)}
  deriving (Eq, Typeable)

-- |
-- Pitch type.
newtype Pitch = Pitch {getPitch :: Interval}
  deriving (Eq, Ord, Typeable)

instance Semigroup Interval where
  (<>) = (^+^)

instance Monoid Interval where

  mempty = basis_P1

  mappend = (^+^)

instance VectorSpace Interval where

  type Scalar Interval = Integer

  (*^) = stackInterval
    where
      stackInterval n a
        | n >= 0 = mconcat $ replicate (fromIntegral n) a
        | otherwise = negate $ stackInterval (negate n) a

instance AdditiveGroup Interval where

  zeroV = basis_P1 where basis_P1 = Interval (0, 0)

  (Interval (a1, d1)) ^+^ (Interval (a2, d2)) = Interval (a1 ^+^ a2, d1 ^+^ d2)

  negateV (Interval (a, d)) = Interval (- a, - d)

-- | Avoid using '(*)', or 'signum' on intervals.
instance Num Interval where

  (+) = (^+^)

  negate = negateV

  abs a = if isNegative a then negate a else a

  (*) = error "Music.Pitch.Common.Interval: no overloading for (*)"

  signum = error "Music.Pitch.Common.Interval: no overloading for signum"

  fromInteger = error "Music.Pitch.Common.Interval: no overloading for fromInteger"

instance AffineSpace Pitch where

  type Diff Pitch = Interval

  Pitch a .-. Pitch b = a ^-^ b

  Pitch a .+^ b = Pitch (a ^+^ b)

instance Show Octaves where
  show = show . getOctaves

instance Show Number where
  show = show . getNumber

instance Show Accidental where
  show n
    | n == 0 = "natural"
    | n == 1 = "sharp"
    | n == (-1) = "flat"
    | n == 2 = "doubleSharp"
    | n == (-2) = "doubleFlat"
    | n > 0 = "sharp * " ++ show (getAccidental n)
    | n < 0 = "flat * " ++ show (negate $ getAccidental n)

instance Alterable Accidental where

  sharpen = succ

  flatten = pred

-- -- | Magic instance that allow us to write @c sharp@ instead of @sharpen c@.
-- instance (IsPitch a, Alterable a) => IsPitch (Accidental -> a) where
--   fromPitch l 1     = sharpen (fromPitch l)
--   fromPitch l (-1)  = flatten (fromPitch l)
-- Requires FlexibleInstances

-- | Lexicographical ordering, comparing the 'd2' component of the
-- Interval first, as it's tied to the Number which is expected to be
-- 'bigger' than the Quality, assuming ordinary tuning systems
instance Ord Interval where
  Interval a `compare` Interval b = swap a `compare` swap b
    where
      swap (x, y) = (y, x)

instance AdditiveGroup ChromaticSteps where

  zeroV = 0

  (^+^) = (+)

  negateV = negate

instance AdditiveGroup DiatonicSteps where

  zeroV = 0

  (^+^) = (+)

  negateV = negate
{-
The number portion of an interval (i.e. second, third, etc).

Note that the interval number is always one step larger than number of steps spanned by
the interval (i.e. a third spans two diatonic steps). Thus 'number' does not distribute
over addition:

> number (a + b) = number a + number b - 1
-}

{-
Common pitch representation.

Intervals and pitches can be added using '.+^'. To get the interval between
two pitches, use '.-.'.

Pitches are normally entered using the following literals.

> c d e f g a b

Notes with accidentals can be written by adding the @s@ or @b@ suffices
(or two for double sharps and flats).

> cs, ds, es ...    -- sharp
> cb, db, eb ...    -- flat
> css, dss, ess ... -- double sharp
> cbb, dbb, ebb ... -- double flat

There is also a convenience syntax for entering pitches one octave up or
down, using @'@ and @_@ respectively.

> g a b c'
> d c b_ c

Because of some overloading magic, we can actually write @sharp@ and
@flat@ as /postfix/ functions. This gives a better read:

> cs == c sharp
> db == c flat

You can of course use typical functional transformation of pitch as well.
For example 'sharpen' and 'flatten' are the ordinary (prefix) versions of
'sharp' and 'flat'

> sharpen c             == c sharp       == cs
> flatten d             == d flat        == ds
> (sharpen . sharpen) c == c doubleSharp == css
> (flatten . flatten) d == d doubleFlat  == dss

Note that there is no guarantee that your pitch representation use
enharmonic equivalence, so @cs == db@ may or may not hold.

> c .+^ minor third == eb
> f .-. c           == perfect fourth

Pitches are described by name, accidental and octave number.

> c   == fromIntegral 0
> _P4 == perfect fourth   == interval Perfect 5
> d5  == diminished fifth == diminish (perfect fifth)

-}

{-
A musical interval such as minor third, augmented fifth, duodecim etc.

We include direction in in this definition, so a downward minor third (written @-m3@)
is distinct from an upward minor third (written @m3@). Note that @_P1@ and @-P1@ are
synynoms.

Not to be confused with a mathematical inverval in pitch space, which is called
'Ambitus'. Intervals and pitches form an affine-vector space pair with intervals and
/vectors/ and pitches as /points/. To add an interval to a, use '.+^'. To get the
interval between two pitches, use '.-.'.

> c .+^ minor third == eb
> f .-. c           == perfect fourth

Adding intervals preserves spelling. For example:

> m3 ^+^ _M3 = _P5
> d5 ^+^ _M6 = m10

The scalar type of 'Interval' is 'Int', using '^*' to stack intervals of a certain type
on top of each other. For example @_P5 ^* 2@ is a stack of 2 perfect fifths, or a major
ninth. The 'Num' instance works as expected for '+', 'negate' and 'abs', and
(arbitrarily) uses minor seconds for multiplication. If you find yourself '*', or
'signum' on intervals, consider switching to '*^' or 'normalized'.

Intervals are generally described in terms of 'Quality' and 'Number'. To construct an
interval, use the 'interval' constructor, the utility constructors 'major', 'minor',
'augmented' and 'diminished', or the interval literals:

> m5  == minor   fifth    == interval Minor   5 > _P4 == perfect fourth   == interval
Perfect 5 > d5  == diminished fifth == diminish (perfect fifth)

-}

{-
An accidental is either flat, natural or sharp.

This representation allows for an arbitrary number of flats or sharps rather than just
single and double.

The 'Num' and 'Enum' instances treat 'Accidental' as the number of altered semitones,
i.e. a double flat is @-2@, natural @0@ and so on.

-}

{-
Interval quality is either perfect, major, minor, augmented, and
diminished. This representation allows for an arbitrary number of
augmentations or diminutions, so /augmented/ is represented by @Augmented
1@, /doubly augmented/ by @Augmented 2@ and so on.

The quality of a compound interval is the quality of the simple interval on
which it is based.

-}

{-
An interval represented as a number of octaves, including negative
intervals.

> octaves a = semitones a `div` 12
> steps   a = semitones a `mod` 12
-}

basis_P1 = Interval (0, 0)

basis_A1 = Interval (1, 0)

basis_d2 = Interval (0, 1)

basis_P5 = Interval (7, 4)

basis_P8 = Interval (12, 7)

-- |
-- Returns whether the given interval is negative.
isNegative :: Interval -> Bool
isNegative (Interval (a, d)) = d < 0

-- |
-- Returns whether the given interval is positive.
isPositive :: Interval -> Bool
isPositive x@(Interval (a, d)) = d >= 0 && not (isPerfectUnison x)


instance Show Interval where
  show a
    | isNegative a = "-" ++ showQuality (extractQuality a) ++ show (abs $ extractNumber a)
    | otherwise = showQuality (extractQuality a) ++ show (abs $ extractNumber a)
    where
      showQuality Major = "_M"
      showQuality Minor = "m"
      showQuality Perfect = "_P"
      showQuality (Augmented n) = "_" ++ replicate (fromIntegral n) 'A'
      showQuality (Diminished n) = replicate (fromIntegral n) 'd'

instance HasBasis Interval where

  type Basis Interval = IntervalBasis

  basisValue Chromatic = basis_A1
  basisValue Diatonic = basis_d2

  decompose (Interval (c, d)) = [(Chromatic, fromIntegral c), (Diatonic, fromIntegral d)]

  decompose' (Interval (c, d)) Chromatic = fromIntegral c
  decompose' (Interval (c, d)) Diatonic = fromIntegral d


instance HasQuality Interval where
  quality i = extractQuality i

instance HasNumber Interval where
  number i = extractNumber i

instance Augmentable Interval where

  augment i = i ^+^ basis_A1

  diminish i = i ^-^ basis_A1

instance HasSemitones Interval where
  semitones (Interval (a, d)) = fromIntegral a -- assuming "semitone" == A1

instance ToJSON DiatonicSteps where
  toJSON = toJSON . toInteger

instance FromJSON DiatonicSteps where
  parseJSON = fmap fromInteger . parseJSON

instance ToJSON ChromaticSteps where
  toJSON = toJSON . toInteger

instance FromJSON ChromaticSteps where
  parseJSON = fmap fromInteger . parseJSON

instance ToJSON Interval where
  toJSON i = Data.Aeson.object [("steps", toJSON $ i ^. _steps), ("alteration", toJSON $ i ^. _alteration)]

instance FromJSON Interval where
  parseJSON (Data.Aeson.Object x) = liftA2 (curry (^. intervalAlterationSteps)) alteration steps
    where
      steps = x Data.Aeson..: "steps"
      alteration = x Data.Aeson..: "alteration"
  parseJSON _ = empty

intervalDiff :: Interval -> Int
intervalDiff (Interval (c, d)) = fromIntegral $ c - fromIntegral (diatonicToChromatic d)

-- |
-- Creates an interval from a quality and number.
--
-- Given 'Perfect' with an number not indicating a perfect consonant, 'interval' returns a
-- major interval instead. Given 'Major' or 'Minor' with a number indicating a perfect
-- consonance, 'interval' returns a perfect or diminished interval respectively.
mkInterval' ::
  -- | Difference in chromatic steps (?).
  Int ->
  -- | Number of diatonic steps (NOT interval number).
  Int ->
  Interval
mkInterval' diff diatonic = Interval (diatonicToChromatic (fromIntegral diatonic) + fromIntegral diff, fromIntegral diatonic)

-- |
-- Extracting the 'number' from an interval vector.
--
-- Note that (a, d) is a representation of the interval (a * A1) + (d
-- * d2), so the 'number' part of the interval must be stored entirely
-- in the d * d2 part (adding a unison, perfect or otherwise, can
-- never increase the number of the interval)
extractNumber :: Interval -> Number
extractNumber (Interval (a, d))
  | d >= 0 = fromIntegral (d + 1)
  | otherwise = fromIntegral (d - 1)

-- |
-- Extracting the 'quality' from an interval vector.
--
-- This is much more finicky, as the A1 and d2 intervals interact in a
-- complex way to produce the perfect/major/minor/etc. intervals that
-- we are used to reading.
extractQuality :: Interval -> Quality
extractQuality (Interval (a, d))
  | (a < 0) && (d == 0) = diminish $ extractQuality (Interval ((a + 1), d))
  | (a, d) == (0, 0) = Perfect
  | (a > 0) && (d == 0) = augment $ extractQuality (Interval ((a - 1), d))
  | (a < 1) && (d == 1) = diminish $ extractQuality (Interval ((a + 1), d))
  | (a, d) == (1, 1) = Minor
  | (a, d) == (2, 1) = Major
  | (a > 2) && (d == 1) = augment $ extractQuality (Interval ((a - 1), d))
  | (a < 3) && (d == 2) = diminish $ extractQuality (Interval ((a + 1), d))
  | (a, d) == (3, 2) = Minor
  | (a, d) == (4, 2) = Major
  | (a > 4) && (d == 2) = augment $ extractQuality (Interval ((a - 1), d))
  | (a < 5) && (d == 3) = diminish $ extractQuality (Interval ((a + 1), d))
  | (a, d) == (5, 3) = Perfect
  | (a > 5) && (d == 3) = augment $ extractQuality (Interval ((a - 1), d))
  | (a < 7) && (d == 4) = diminish $ extractQuality (Interval ((a + 1), d))
  | (a, d) == (7, 4) = Perfect
  | (a > 7) && (d == 4) = augment $ extractQuality (Interval ((a - 1), d))
  | (a < 8) && (d == 5) = diminish $ extractQuality (Interval ((a + 1), d))
  | (a, d) == (8, 5) = Minor
  | (a, d) == (9, 5) = Major
  | (a > 9) && (d == 5) = augment $ extractQuality (Interval ((a - 1), d))
  | (a < 10) && (d == 6) = diminish $ extractQuality (Interval ((a + 1), d))
  | (a, d) == (10, 6) = Minor
  | (a, d) == (11, 6) = Major
  | (a > 11) && (d == 6) = augment $ extractQuality (Interval ((a - 1), d))
  | (a < 12) && (d == 7) = diminish $ extractQuality (Interval ((a + 1), d))
  | (a, d) == (12, 7) = Perfect
  | (a > 12) && (d == 7) = augment $ extractQuality (Interval ((a - 1), d))
  -- note: these last two cases *have* to be this way round, otherwise
  -- infinite loop occurs.
  | (a > 12) || (d > 7) = extractQuality (Interval ((a - 12), (d - 7)))
  | (a < 0) || (d < 0) = extractQuality (Interval ((- a), (- d)))

-- | Creates a perfect interval.
--   If given an inperfect number, constructs a major interval.
perfect :: Number -> Interval
perfect = mkInterval Perfect

-- | Creates a major interval.
--   If given a perfect number, constructs a perfect interval.
major :: Number -> Interval
major = mkInterval Major

-- | Creates a minor interval.
--   If given a perfect number, constructs a diminished interval.
minor :: Number -> Interval
minor = mkInterval Minor

-- | Creates an augmented interval.
augmented :: Number -> Interval
augmented = mkInterval (Augmented 1)

-- | Creates a diminished interval.
diminished :: Number -> Interval
diminished = mkInterval (Diminished 1)

-- | Creates a doubly augmented interval.
doublyAugmented :: Number -> Interval
doublyAugmented = mkInterval (Augmented 2)

-- | Creates a doubly diminished interval.
doublyDiminished :: Number -> Interval
doublyDiminished = mkInterval (Diminished 2)

-- |
-- Separate a compound interval into octaves and a simple interval.
--
-- > x*^_P8 + y = z  iff  (x, y) = separate z
--
-- >>> separate (2*^_P8+m3)
-- (2,m3)
-- >>> separate (3*^_P8+m3)
-- (3,m3)
-- >>> separate (0*^_P8+m3)
-- (0,m3)
-- >>> separate ((-1)*^_P8+m3)
separate :: Interval -> (Octaves, Interval)
separate i = (fromIntegral o, i ^-^ (fromIntegral o *^ basis_P8))
  where
    o = octaves i


-- |
-- Returns the simple part of an interval.
--
-- > (perfect octave)^*x + y = z  iff  y = simple z
simple :: Interval -> Interval
simple = snd . separate

-- |
-- Returns whether the given interval is simple.
--
-- A simple interval is a non-negative interval spanning less than one octave.
isSimple :: Interval -> Bool
isSimple x = octaves x == 0

-- |
-- Returns whether the given interval is compound.
--
-- A compound interval is either a negative interval, or a positive interval spanning
-- one octave or more. Note that compound intervals may be smaller than an octave if
-- they are negative, so
--
-- >>> isCompound (-m3)
-- True
-- >>> isCompound $ abs (-m3)
-- False
isCompound :: Interval -> Bool
isCompound x = octaves x /= 0


-- |
-- Returns whether the given interval is non-negative. This implies that it is either positive or a perfect unison.
isNonNegative :: Interval -> Bool
isNonNegative (Interval (a, d)) = d >= 0

-- |
-- Returns whether the given interval a perfect unison.
isPerfectUnison :: Interval -> Bool
isPerfectUnison (Interval (a, d)) = (a, d) == (0, 0)

-- |
-- Returns whether the given interval is a step (a second or smaller).
--
-- Only diatonic 'number' is taken into account, so @_A2@ is considered
-- a step and @m3@ a leap, even though they have the same number of
-- semitones.
isStep :: Interval -> Bool
isStep (Interval (a, d)) = (abs d) <= 1

-- |
-- Returns whether the given interval is a leap (larger than a second).
--
-- Only the diatonic 'number' is taken into account, so @_A2@ is considered
-- a step and @m3@ a leap, even though they have the same number of
-- semitones.
isLeap :: Interval -> Bool
isLeap (Interval (a, d)) = (abs d) > 1

-- |
-- Intervallic inversion.
--
-- The inversion an interval is determined as follows:
--
-- * The number of a simple interval the difference of nine and the number of its inversion.
--
-- * The quality of a simple interval is the inversion of the quality of its inversion.
--
-- * The inversion of a compound interval is the inversion of its simple component.
invert :: Interval -> Interval
invert = simple . negate

mkInterval :: Quality -> Number -> Interval
mkInterval q n = mkInterval' (fromIntegral diff) (fromIntegral steps)
  where
    diff = qualityToDiff (n > 0) (expectedQualityType n) (q)
    steps = case n `compare` 0 of
      GT -> n - 1
      EQ -> error "diatonicSteps: Invalid number 0"
      LT -> n + 1

-- steps = n^.diatonicSteps

-- | View or set the alteration (i.e. the number of chromatic steps differing from the excepted number) in an interval.
_alteration :: Lens' Interval ChromaticSteps
_alteration = from intervalAlterationSteps . _1

-- | View or set the number of chromatic steps in an interval.
_steps :: Lens' Interval DiatonicSteps
_steps = from intervalAlterationSteps . _2

-- | View or set the quality of an interval.
_quality :: Lens' Interval Quality
_quality = from interval . _1

-- | View or set the number component of an interval.
_number :: Lens' Interval Number
_number = from interval . _2

-- | View an interval as a pair of quality and number or vice versa.
interval :: Iso' (Quality, Number) Interval
interval = iso (uncurry mkInterval) (\x -> (quality x, number x))

-- | View an interval as a pair of alteration and diatonic steps or vice versa.
--
-- >>> _P5^.from intervalAlterationSteps
-- (ChromaticSteps {getChromaticSteps = 0},DiatonicSteps {getDiatonicSteps = 4})
--
-- >>> d5^.from intervalAlterationSteps
-- (ChromaticSteps {getChromaticSteps = -1},DiatonicSteps {getDiatonicSteps = 4})
intervalAlterationSteps :: Iso' (ChromaticSteps, DiatonicSteps) Interval
intervalAlterationSteps =
  iso
    (\(d, s) -> mkInterval' (fromIntegral d) (fromIntegral s))
    (\x -> (qualityToDiff (number x >= 0) (expectedQualityType (number x)) (quality x), (number x) ^. diatonicSteps))

-- TODO rename this
-- | View an interval as a pair of total number of chromatic and diatonic steps.
interval' :: Iso' (ChromaticSteps, DiatonicSteps) Interval
interval' = iso Interval getInterval


-- |
-- TODO>>> m3 & _number %~ pred
-- m2
-- TODO>>> m3 & _number %~ succ
-- d4
-- TODO>>> _M3 & _number %~ succ
-- _P4
--
--
-- >>> m3 & _number +~ 1
-- d4
-- >>> m3 & _number +~ 2
-- d5
-- >>> m3 & _number +~ 3
-- m6
-- >>> m3 & _number +~ 4
--
--
-- >>> m3 & _quality .~ Minor
-- m3
-- >>> _P5 & _quality .~ Minor
-- d5
-- >>> (-d5) & _quality %~ diminish
--
--
-- TODO only obeys lens laws up to quality normalization
--
-- >>> _P5 & _quality .~ Minor
-- d5
-- >>> _P5 & _quality .~ (Diminished 1)
-- d5

-- Internal stuff

-- TODO more generic pattern here
diatonicToChromatic :: DiatonicSteps -> ChromaticSteps
diatonicToChromatic d = fromIntegral $ (octaves * 12) + go restDia
  where
    -- restDia is always in [0..6]
    (octaves, restDia) = fromIntegral d `divMod` 7
    go = ([0, 2, 4, 5, 7, 9, 11] !!)

-- | Integer div of intervals: i / di = x, where x is an integer
intervalDiv :: Interval -> Interval -> Int
intervalDiv (Interval (a, d)) (Interval (1, 0)) = fromIntegral a
intervalDiv (Interval (a, d)) (Interval (0, 1)) = fromIntegral d
intervalDiv i di
  | (i > basis_P1) = intervalDivPos i di
  | (i < basis_P1) = intervalDivNeg i di
  | otherwise = 0 :: Int
  where
    intervalDivPos i di
      | (i < basis_P1) = error "Impossible"
      | (i ^-^ di) < basis_P1 = 0
      | otherwise = 1 + (intervalDiv (i ^-^ di) di)
    intervalDivNeg i di
      | (i > basis_P1) = error "Impossible"
      | (i ^+^ di) > basis_P1 = 0
      | otherwise = 1 + (intervalDiv (i ^+^ di) di)

{- TODO remove after checking these are the same as the above
 -
-- TODO more generic pattern here
diatonicToChromatic :: DiatonicSteps -> ChromaticSteps
diatonicToChromatic d = fromIntegral $ (octaves * 12) + go restDia
  where
    -- restDia is always in [0..6]
    (octaves, restDia) = fromIntegral d `divMod` 7
    go = ([0, 2, 4, 5, 7, 9, 11] !!)

-- | Integer div of intervals: i / di = x, where x is an integer
intervalDiv :: Interval -> Interval -> Int
intervalDiv (Interval (a, d)) (Interval (1, 0)) = fromIntegral a
intervalDiv (Interval (a, d)) (Interval (0, 1)) = fromIntegral d
intervalDiv i di
  | (i > basis_P1) = intervalDivPos i di
  | (i < basis_P1) = intervalDivNeg i di
  | otherwise = 0 :: Int
  where
    intervalDivPos i di
      | (i < basis_P1) = error "Impossible"
      | (i ^-^ di) < basis_P1 = 0
      | otherwise = 1 + (intervalDiv (i ^-^ di) di)
    intervalDivNeg i di
      | (i > basis_P1) = error "Impossible"
      | (i ^+^ di) > basis_P1 = 0
      | otherwise = 1 + (intervalDiv (i ^+^ di) di)
-}


-- | Represent an interval i in a new basis (j, k).
--
-- We want x,y where i = x*j + y*k
--
-- e.g., convertBasis basis_d2 _P5 basis_P8 == Just (-12,7), as expected.
convertBasis ::
  Interval ->
  Interval ->
  Interval ->
  Maybe (Int, Int)
convertBasis i j k
  | (p == 0) = Nothing
  | not $ p `divides` r = Nothing
  | not $ p `divides` q = Nothing
  | otherwise = Just (r `div` p, q `div` p)
  where
    Interval (fromIntegral -> m, fromIntegral -> n) = i
    Interval (fromIntegral -> a, fromIntegral -> b) = j
    Interval (fromIntegral -> c, fromIntegral -> d) = k
    p = (a * d - b * c)
    q = (a * n - b * m)
    r = (d * m - c * n)
    divides :: Integral a => a -> a -> Bool
    x `divides` y = (y `rem` x) == 0

-- | Same as above, but don't worry if new interval has non-integer
-- coefficients -- useful when getting a value to use as a frequency
-- ratio in a tuning system.
convertBasisFloat ::
  (Fractional t, Eq t) =>
  Interval ->
  Interval ->
  Interval ->
  Maybe (t, t)
convertBasisFloat i j k
  | (p == 0) = Nothing
  | otherwise = Just (r / p, q / p)
  where
    Interval (fromIntegral -> m, fromIntegral -> n) = i
    Interval (fromIntegral -> a, fromIntegral -> b) = j
    Interval (fromIntegral -> c, fromIntegral -> d) = k
    p = fromIntegral $ (a * d - b * c)
    q = fromIntegral $ (a * n - b * m)
    r = fromIntegral $ (d * m - c * n)

{- TODO remove after checking they are the same as the above
 -
-- | Represent an interval i in a new basis (j, k).
--
-- We want x,y where i = x*j + y*k
--
-- e.g., convertBasis basis_d2 _P5 basis_P8 == Just (-12,7), as expected.
convertBasis ::
  Interval ->
  Interval ->
  Interval ->
  Maybe (Int, Int)
convertBasis i j k
  | (p == 0) = Nothing
  | not $ p `divides` r = Nothing
  | not $ p `divides` q = Nothing
  | otherwise = Just (r `div` p, q `div` p)
  where
    Interval (fromIntegral -> m, fromIntegral -> n) = i
    Interval (fromIntegral -> a, fromIntegral -> b) = j
    Interval (fromIntegral -> c, fromIntegral -> d) = k
    p = (a * d - b * c)
    q = (a * n - b * m)
    r = (d * m - c * n)
    divides :: Integral a => a -> a -> Bool
    x `divides` y = (y `rem` x) == 0

-- | Same as above, but don't worry if new interval has non-integer
-- coefficients -- useful when getting a value to use as a frequency
-- ratio in a tuning system.
convertBasisFloat ::
  (Fractional t, Eq t) =>
  Interval ->
  Interval ->
  Interval ->
  Maybe (t, t)
convertBasisFloat i j k
  | (p == 0) = Nothing
  | otherwise = Just (r / p, q / p)
  where
    Interval (fromIntegral -> m, fromIntegral -> n) = i
    Interval (fromIntegral -> a, fromIntegral -> b) = j
    Interval (fromIntegral -> c, fromIntegral -> d) = k
    p = fromIntegral $ (a * d - b * c)
    q = fromIntegral $ (a * n - b * m)
    r = fromIntegral $ (d * m - c * n)
-}



instance HasOctaves Interval where
  -- |
  -- Returns the non-simple part of an interval.
  --
  -- > _P8^*octaves x ^+^ simple x = x
  octaves :: Interval -> Octaves
  octaves (Interval (_, d)) = fromIntegral $ d `div` 7





{-
Note: This is *not* the same as wrapping/unwrapping, as the number of chromatic steps viewed here is
an *alteration*, rather than the total number of chromatic steps (basis d2).

E.g. d5 is internally represented as (6,4) but _P5^.from interval' == (-1,4).
-}

-- |
-- >>> m3 & _number %~ pred
-- m2
-- >>> m3 & _number %~ succ
-- d4
-- >>> _M3 & _number %~ succ
-- _P4
--
--
-- >>> m3 & _number +~ 1
-- d4
-- >>> m3 & _number +~ 2
-- d5
-- >>> m3 & _number +~ 3
-- m6
-- >>> m3 & _number +~ 4
--
--
-- >>> m3 & _quality .~ Minor
-- m3
-- >>> _P5 & _quality .~ Minor
-- d5
-- >>> (-d5) & _quality %~ diminish
--
--
-- TODO only obeys lens laws up to quality normalization
--
-- >>> _P5 & _quality .~ Minor
-- d5
-- >>> _P5 & _quality .~ (Diminished 1)
-- d5

-- Internal stuff



-- |
-- Class of intervals that has a number of 'Octaves'.
class HasOctaves a where
  -- |
  -- Returns the number of octaves spanned by an interval.
  --
  -- The number of octaves is negative if and only if the interval is
  -- negative.
  --
  -- Examples:
  --
  -- > octaves (perfect unison)  =  0
  -- > octaves (d5 ^* 4)         =  2
  -- > octaves (-_P8)            =  -1
  octaves :: a -> Octaves

instance HasOctaves Octaves where octaves = id

{-
-- |
-- Class of intervals that has a number of 'Steps'.
--
class HasSteps a where
  -- |
  -- The number of steps is always in the range /0 ≤ x < 12/.
  --
  -- Examples:
  --
  -- > octaves (perfect unison)  =  0
  -- > octaves (d5 ^* 4)         =  2
  -- > octaves (-m7)             =  -1
  --
  steps :: a -> Steps
-}

-- |
-- Class of intervals that can be converted to a number of 'Semitones'.
class HasSemitones a where
  -- |
  -- Returns the number of semitones spanned by an interval.
  --
  -- The number of semitones is negative if and only if the interval is
  -- negative.
  --
  -- >>> semitones (perfect unison)
  -- 0
  -- >>> semitones tritone
  -- 6
  -- >>> semitones d5
  -- 6
  -- >>> semitones (-_P8)
  -- -12
  semitones :: a -> Semitones

instance HasSemitones ChromaticSteps where semitones = id

semitone, tone, ditone, tritone :: Semitones

-- | Precisely one semitone.
semitone = 1

-- | Precisely one whole tone, or two semitones.
tone = 2

-- | Precisely two whole tones, or four semitones.
ditone = 4

-- | Precisely three whole tones, or six semitones.
tritone = 6

isTone, isSemitone, isTritone :: HasSemitones a => a -> Bool

-- | Returns true iff the given interval spans one semitone.
isSemitone = (== semitone) . abs . semitones

-- | Returns true iff the given interval spans one whole tone (two semitones).
isTone = (== tone) . abs . semitones

-- | Returns true iff the given interval spans three whole tones (six semitones).
isTritone = (== tritone) . abs . semitones

infix 4 =:=

infix 4 /:=

-- |
-- Enharmonic equivalence.
--
-- >>> asInterval _A2 == m3
-- False
-- >>> asInterval _A2 =:= m3
-- True
(=:=) :: HasSemitones a => a -> a -> Bool
a =:= b = semitones a == semitones b

-- |
-- Enharmonic non-equivalence.
--
-- >>> asInterval _A2 /= m3
-- True
-- >>> asInterval _A2 /:= m3
-- False
(/:=) :: HasSemitones a => a -> a -> Bool
a /:= b = semitones a /= semitones b

-- | Types of value that has an interval quality (mainly 'Interval' and 'Quality' itself).
class HasQuality a where
  quality :: a -> Quality

-- |
-- Returns whether the given quality is perfect.
isPerfect :: HasQuality a => a -> Bool
isPerfect a = case quality a of Perfect -> True; _ -> False

-- |
-- Returns whether the given quality is major.
isMajor :: HasQuality a => a -> Bool
isMajor a = case quality a of Major -> True; _ -> False

-- |
-- Returns whether the given quality is minor.
isMinor :: HasQuality a => a -> Bool
isMinor a = case quality a of Minor -> True; _ -> False

-- |
-- Returns whether the given quality is augmented (including double augmented etc).
isAugmented :: HasQuality a => a -> Bool
isAugmented a = case quality a of Augmented _ -> True; _ -> False

-- |
-- Returns whether the given quality is diminished (including double diminished etc).
isDiminished :: HasQuality a => a -> Bool
isDiminished a = case quality a of Diminished _ -> True; _ -> False

{-
Is this quality standard, i.e. major, minor, perfect, augmented/diminished
or doubly augmented/diminished.
-}
isStandardQuality :: Quality -> Bool
isStandardQuality (Augmented n) = n <= 2
isStandardQuality (Diminished n) = n <= 2
isStandardQuality _ = True

{-
Same as 'isStandardQuality' but disallow doubly augmented/diminished.
-}
isSimpleQuality :: Quality -> Bool
isSimpleQuality (Augmented n) = n <= 1
isSimpleQuality (Diminished n) = n <= 1
isSimpleQuality _ = True

instance HasQuality Quality where
  quality = id

-- | Augmentable Quality instance
--
-- This Augmentable instance exists solely for use of the extractQuality
-- function, which ensures that there is never any ambiguity around
-- diminished/augmented intervals turning into major/minor/perfect
-- intervals.
instance Augmentable Quality where

  augment Major = Augmented 1
  augment Minor = Major
  augment Perfect = Augmented 1
  augment (Augmented n) = Augmented (n + 1)
  augment (Diminished n) = Diminished (n - 1)

  diminish Major = Minor
  diminish Minor = Diminished 1
  diminish Perfect = Diminished 1
  diminish (Augmented n) = Augmented (n - 1)
  diminish (Diminished n) = Diminished (n + 1)

-- |
-- Invert a quality.
--
-- Perfect is unaffected, major becomes minor and vice versa, augmented
-- becomes diminished and vice versa.
invertQuality :: Quality -> Quality
invertQuality = go
  where
    go Major = Minor
    go Minor = Major
    go Perfect = Perfect
    go (Augmented n) = Diminished n
    go (Diminished n) = Augmented n

-- |
-- The quality type expected for a given number, i.e. perfect for unisons, fourths,
-- fifths and their compounds; major/minor for everything else.
expectedQualityType :: Number -> QualityType
expectedQualityType x =
  if ((abs x - 1) `mod` 7) + 1 `elem` [1, 4, 5]
    then PerfectType
    else MajorMinorType

-- |
-- Return all possible quality types for a given quality.
qualityTypes :: Quality -> [QualityType]
qualityTypes Perfect = [PerfectType]
qualityTypes Major = [MajorMinorType]
qualityTypes Minor = [MajorMinorType]
qualityTypes _ = [PerfectType, MajorMinorType]

-- |
-- Return whether the given combination of quality and number forms a valid interval
-- expression.
isValidQualityNumber :: Quality -> Number -> Bool
isValidQualityNumber q n = expectedQualityType n `elem` qualityTypes q

data Direction = Upward | Downward
  deriving (Eq, Ord, Show)

-- |
-- Return the alteration in implied by the given quality to a number of a given quality type.
qualityToAlteration :: Direction -> QualityType -> Quality -> Maybe ChromaticSteps
qualityToAlteration d qt q = fmap fromIntegral $ go d qt q
  where
    go Upward MajorMinorType (Augmented n) = Just $ 0 + n
    go Upward MajorMinorType Major = Just $ 0
    go Upward MajorMinorType Minor = Just $ (-1)
    go Upward MajorMinorType (Diminished n) = Just $ - (1 + n)
    go Downward MajorMinorType (Augmented n) = Just $ - (1 + n)
    go Downward MajorMinorType Major = Just $ -1
    go Downward MajorMinorType Minor = Just $ 0
    go Downward MajorMinorType (Diminished n) = Just $ 0 + n
    go Upward PerfectType (Augmented n) = Just $ 0 + n
    go Upward PerfectType Perfect = Just $ 0
    go Upward PerfectType (Diminished n) = Just $ 0 - n
    go Downward PerfectType (Augmented n) = Just $ 0 - n
    go Downward PerfectType Perfect = Just $ 0
    go Downward PerfectType (Diminished n) = Just $ 0 + n
    go _ qt q = Nothing

qualityToDiff x qt q = fromMaybe e $ qualityToAlteration (f x) qt q
  where
    f True = Upward
    f False = Downward
    e =
      error $
        "qualityToDiff: Unknown interval expression ("
          ++ show qt
          ++ ", "
          ++ show q
          ++ ")"
{-# DEPRECATED qualityToDiff "Use qualityToAlteration" #-}


instance HasNumber Number where number = id

-- | A synonym for @1@.
unison :: Number
unison = 1

-- | A synonym for @1@.
prime :: Number
prime = 1

-- | A synonym for @2@.
second :: Number
second = 2

-- | A synonym for @3@.
third :: Number
third = 3

-- | A synonym for @4@.
fourth :: Number
fourth = 4

-- | A synonym for @5@.
fifth :: Number
fifth = 5

-- | A synonym for @6@.
sixth :: Number
sixth = 6

-- | A synonym for @7@.
seventh :: Number
seventh = 7

-- | A synonym for @8@.
octave :: Number
octave = 8

-- | A synonym for @9@.
ninth :: Number
ninth = 9

-- | A synonym for @10@.
tenth :: Number
tenth = 10

-- | A synonym for @11@.
eleventh :: Number
eleventh = 11

-- | A synonym for @12@.
twelfth :: Number
twelfth = 12

-- | A synonym for @12@.
duodecim :: Number
duodecim = 12

-- | A synonym for @13@.
thirteenth :: Number
thirteenth = 13

-- | A synonym for @14@.
fourteenth :: Number
fourteenth = 14

-- | A synonym for @15@.
fifteenth :: Number
fifteenth = 15

class HasNumber a where
  -- |
  -- Returns the number portion of an interval.
  --
  -- The interval number is negative if and only if the interval is negative.
  --
  -- See also 'quality', 'octaves' and 'semitones'.
  number :: a -> Number

-- TODO rename numberDiatonicSteps
diatonicSteps :: Iso' Number DiatonicSteps
diatonicSteps = iso n2d d2n
  where
    n2d n | n > 0 = fromIntegral (n - 1)
    n2d n | n == 0 = error "diatonicSteps: Invalid number 0"
    n2d n | n < 0 = fromIntegral (n + 1)
    d2n n | n >= 0 = fromIntegral (n + 1)
    d2n n | n < 0 = fromIntegral (n - 1)

newtype IntervalL = IntervalL (Integer, Integer, Integer)

-- (octaves, diatonic steps, chromatic steps)

class IsInterval a where
  fromInterval :: Interval -> a

instance IsInterval Interval where
  fromInterval = id

instance IsInterval a => IsInterval (Maybe a) where
  fromInterval = pure . fromInterval

instance IsInterval a => IsInterval (First a) where
  fromInterval = pure . fromInterval

instance IsInterval a => IsInterval (Last a) where
  fromInterval = pure . fromInterval

instance IsInterval a => IsInterval [a] where
  fromInterval = pure . fromInterval

instance (Monoid b, IsInterval a) => IsInterval (b, a) where
  fromInterval = pure . fromInterval

deriving instance (Monoid b, IsInterval a) => IsInterval (Couple b a)

instance IsInterval Int where
  fromInterval x = fromIntegral (fromInterval x :: Integer)

instance IsInterval Word where
  fromInterval x = fromIntegral (fromInterval x :: Integer)

instance IsInterval Integer where
  fromInterval (Interval (ChromaticSteps c, _d)) = c

fromIntervalL :: IsInterval a => IntervalL -> a
fromIntervalL = fromInterval . go
  where
    go :: IntervalL -> Interval
    go (IntervalL (o, d, c)) = (basis_P8 ^* o) ^+^ (basis_A1 ^* c) ^+^ (basis_d2 ^* d)

d1 = fromIntervalL $ IntervalL (0, 0, -1)

_P1 = fromIntervalL $ IntervalL (0, 0, 0)

_A1 = fromIntervalL $ IntervalL (0, 0, 1)

d2 = fromIntervalL $ IntervalL (0, 1, 0)

m2 = fromIntervalL $ IntervalL (0, 1, 1)

_M2 = fromIntervalL $ IntervalL (0, 1, 2)

_A2 = fromIntervalL $ IntervalL (0, 1, 3)

d3 = fromIntervalL $ IntervalL (0, 2, 2)

m3 = fromIntervalL $ IntervalL (0, 2, 3)

_M3 = fromIntervalL $ IntervalL (0, 2, 4)

_A3 = fromIntervalL $ IntervalL (0, 2, 5)

d4 = fromIntervalL $ IntervalL (0, 3, 4)

_P4 = fromIntervalL $ IntervalL (0, 3, 5)

_A4 = fromIntervalL $ IntervalL (0, 3, 6)

d5 = fromIntervalL $ IntervalL (0, 4, 6)

_P5 = fromIntervalL $ IntervalL (0, 4, 7)

_A5 = fromIntervalL $ IntervalL (0, 4, 8)

d6 = fromIntervalL $ IntervalL (0, 5, 7)

m6 = fromIntervalL $ IntervalL (0, 5, 8)

_M6 = fromIntervalL $ IntervalL (0, 5, 9)

_A6 = fromIntervalL $ IntervalL (0, 5, 10)

d7 = fromIntervalL $ IntervalL (0, 6, 9)

m7 = fromIntervalL $ IntervalL (0, 6, 10)

_M7 = fromIntervalL $ IntervalL (0, 6, 11)

_A7 = fromIntervalL $ IntervalL (0, 6, 12)

d8 = fromIntervalL $ IntervalL (1, 0, -1)

_P8 = fromIntervalL $ IntervalL (1, 0, 0)

_A8 = fromIntervalL $ IntervalL (1, 0, 1)

d9 = fromIntervalL $ IntervalL (1, 1, 0)

m9 = fromIntervalL $ IntervalL (1, 1, 1)

_M9 = fromIntervalL $ IntervalL (1, 1, 2)

_A9 = fromIntervalL $ IntervalL (1, 1, 3)

d10 = fromIntervalL $ IntervalL (1, 2, 2)

m10 = fromIntervalL $ IntervalL (1, 2, 3)

_M10 = fromIntervalL $ IntervalL (1, 2, 4)

_A10 = fromIntervalL $ IntervalL (1, 2, 5)

d11 = fromIntervalL $ IntervalL (1, 3, 4)

_P11 = fromIntervalL $ IntervalL (1, 3, 5)

_A11 = fromIntervalL $ IntervalL (1, 3, 6)

d12 = fromIntervalL $ IntervalL (1, 4, 6)

_P12 = fromIntervalL $ IntervalL (1, 4, 7)

_A12 = fromIntervalL $ IntervalL (1, 4, 8)

d13 = fromIntervalL $ IntervalL (1, 5, 7)

m13 = fromIntervalL $ IntervalL (1, 5, 8)

_M13 = fromIntervalL $ IntervalL (1, 5, 9)

_A13 = fromIntervalL $ IntervalL (1, 5, 10)

d14 = fromIntervalL $ IntervalL (1, 6, 9)

m14 = fromIntervalL $ IntervalL (1, 6, 10)

_M14 = fromIntervalL $ IntervalL (1, 6, 11)

_A14 = fromIntervalL $ IntervalL (1, 6, 12)

d15 = fromIntervalL $ IntervalL (2, 0, -1)

_P15 = fromIntervalL $ IntervalL (2, 0, 0)

_A15 = fromIntervalL $ IntervalL (2, 0, 1)


