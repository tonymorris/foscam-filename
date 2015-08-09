{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Data.Foscam.File.ImageId(
  ImageId(..)
, AsImageId(..)
, imageId
) where

import Control.Applicative(Applicative((<*>)), (<$>))
import Control.Category(id)
import Control.Lens(Cons(_Cons), Optic', Choice, prism', lens, (^?), ( # ))
import Control.Monad(Monad)
import Data.Digit(Digit, digitC)
import Data.Eq(Eq)
import Data.Foscam.File.Internal(digitCharacter)
import Data.Functor(Functor)
import Data.Maybe(Maybe(Nothing, Just))
import Data.Ord(Ord)
import Data.String(String)
import Data.Traversable(traverse)
import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>), try, many)
import Prelude(Show)

-- $setup
-- >>> import Text.Parsec

data ImageId =
  ImageId
    Digit
    [Digit]
  deriving (Eq, Ord, Show)

class AsImageId p f s where
  _ImageId ::
    Optic' p f s ImageId

instance AsImageId p f ImageId where
  _ImageId =
    id

instance (Choice p, Applicative f) => AsImageId p f String where
  _ImageId =
    prism'
      (\(ImageId h t) -> (digitC #) <$> (h:t))
      (\s -> case s of 
               [] -> 
                 Nothing
               h:t ->
                 let ch = (^? digitC)
                 in ImageId <$> ch h <*> traverse ch t)

instance Cons ImageId ImageId Digit Digit where
  _Cons = 
    prism'
      (\(h, ImageId h' t) -> ImageId h (h':t))
      (\(ImageId h t) -> case t of 
                         [] ->
                           Nothing
                         u:v ->
                           Just (h, ImageId u v))

class AsImageIdHead p f s where
  _ImageIdHead ::
    Optic' p f s Digit

instance AsImageIdHead p f Digit where
  _ImageIdHead =
    id

instance (p ~ (->), Functor f) => AsImageIdHead p f ImageId where
  _ImageIdHead =
    lens
      (\(ImageId h _) -> h)
      (\(ImageId _ t) h -> ImageId h t)

class AsImageIdTail p f s where
  _ImageIdTail ::
    Optic' p f s [Digit]

instance AsImageIdTail p f [Digit] where
  _ImageIdTail =
    id

instance (p ~ (->), Functor f) => AsImageIdTail p f ImageId where
  _ImageIdTail =
    lens
      (\(ImageId _ t) -> t)
      (\(ImageId h _) t -> ImageId h t)

-- |
--
-- >>> parse imageId "test" "2062"
-- Right (ImageId 2 [0,6,2])
--
-- >>> parse imageId "test" "2"
-- Right (ImageId 2 [])
--
-- >>> parse imageId "test" "a"
-- Left "test" (line 1, column 2):
-- expecting image ID
-- not a digit: a
--
-- >>> parse imageId "test" ""
-- Left "test" (line 1, column 1):
-- unexpected end of input
-- expecting image ID
imageId ::
  (Monad f, CharParsing f) =>
  f ImageId
imageId =
  let i = try digitCharacter
  in ImageId <$> i <*> many i <?> "image ID"
