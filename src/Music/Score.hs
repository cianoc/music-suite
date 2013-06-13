
{-# LANGUAGE
    TypeFamilies,
    DeriveFunctor,
    DeriveFoldable,     
    DeriveDataTypeable,
    GeneralizedNewtypeDeriving,
    FlexibleInstances,
    FlexibleContexts,
    ConstraintKinds,
    TypeOperators,    
    OverloadedStrings,
    NoMonomorphismRestriction #-}

-------------------------------------------------------------------------------------
-- |
-- Copyright   : (c) Hans Hoglund 2012
--
-- License     : BSD-style
--
-- Maintainer  : hans@hanshoglund.se
-- Stability   : experimental
-- Portability : non-portable (TF,GNTD)
--
-- Provides a musical score representation.
--
-------------------------------------------------------------------------------------

module Music.Score (
        -- * Prerequisites
        module Control.Monad,
        module Control.Monad.Plus,
        module Data.Semigroup,
        module Data.VectorSpace,
        module Data.AffineSpace,

        -- * Basic types
        module Music.Pitch.Literal,
        module Music.Dynamics.Literal,
        module Music.Time,

        -- * Musical container types
        module Music.Score.Track,
        module Music.Score.Voice,
        module Music.Score.Score,

        -- * Manipulation
        module Music.Score.Combinators,
        module Music.Score.Zip,
        module Music.Score.Part,
        module Music.Score.Ties,
        module Music.Score.Pitch,
        module Music.Score.Dynamics,
        module Music.Score.Articulation,
        module Music.Score.Ornaments,

        -- * Export         
        module Music.Score.Export.Midi,
        module Music.Score.Export.Lilypond,
        module Music.Score.Export.MusicXml,
)
where

import Prelude hiding (foldr, concat, foldl, mapM, concatMap, maximum, sum, minimum)

import Data.Semigroup
import Data.Ratio
import Control.Applicative
import Control.Monad hiding (mapM)
import Control.Monad.Plus
import Data.Maybe
import Data.Either
import Data.Foldable
import Data.Typeable
import Data.Traversable
import Data.VectorSpace hiding (Sum, getSum)
import Data.AffineSpace
import Data.Basis

import Music.Time
import Music.Pitch.Literal
import Music.Dynamics.Literal

import Music.Score.Rhythm
import Music.Score.Track
import Music.Score.Voice
import Music.Score.Score
import Music.Score.Combinators
import Music.Score.Zip
import Music.Score.Pitch
import Music.Score.Ties
import Music.Score.Part
import Music.Score.Articulation
import Music.Score.Dynamics
import Music.Score.Ornaments
import Music.Score.Instances
import Music.Score.Export.Midi
import Music.Score.Export.Lilypond
import Music.Score.Export.MusicXml

-------------------------------------------------------------------------------------
-- Test stuff
-------------------------------------------------------------------------------------

{-

type Note = (PartT Int (TieT
    (TremoloT (HarmonicT (SlideT
        (DynamicT (ArticulationT (TextT Integer))))))))

asScore :: Score Note -> Score Note
asScore = id                     

main = openLy $ foo
foo  = asScore $ 
        (accent $ portato $ melody [c,g',fs',b_,c,cs_] `sb` (1/3)) 
    </> stretch 23 c 
    </> (legato $ c |> d |> e `sb` 2)
    where
        sb = flip stretch
-}
