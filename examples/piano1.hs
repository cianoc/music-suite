{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}

import Music.Prelude

main = defaultMain music

music :: Music
music = fmap Just $ renderPatternsAbs $ fmap renderA $ topLevelScore

-- TODO add info
--  Harmony
--  Texture
--  Random seed?
--  Time (for indexing a global Behavior)?
data Block
  = Block
      { col :: Col,
        range :: Range,
        texture :: Texture
      }

data Col = Blue | Brown

data Range = Hi | Lo

data Texture = Ch | Rep

topLevelScore :: Score Block
topLevelScore =
  ( pure
      (Block Brown Hi Rep)
      |> (pure (Block Blue Hi Rep) |* 2)
  )
    <> ( delay 2
           . stretch 2
       )
      (pure (Block Brown Lo Rep) |> (pure (Block Blue Lo Rep) |* 2))

renderA :: (IsPitch a, Transposable a) => Block -> Pattern a
renderA Block {col, range} =
  let transp =
        case range of
          Hi -> up _P8
          Lo -> id
   in transp $
        case col of
          Blue ->
            newPattern [c, d, e] |/ 8
          Brown ->
            newPattern [e, fs] |/ 6
