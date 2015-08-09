{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Foscam.File.Date(
  Date(..)
, AsDate(..)
, date
) where

import Control.Applicative(Applicative((<*>)), (<$>))
import Control.Category(Category(id))
import Control.Lens(Optic', Choice, prism', (^?), (#))
import Control.Monad(Monad)
import Data.Digit(Digit, digitC)
import Data.Eq(Eq)
import Data.Foscam.File.Internal(digitCharacter)
import Data.Maybe(Maybe(Nothing))
import Data.Ord(Ord)
import Data.String(String)
import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>))
import Prelude(Show)

-- $setup
-- >>> import Text.Parsec

data Date =
  Date
    Digit
    Digit
    Digit
    Digit
    Digit
    Digit
    Digit
    Digit
  deriving (Eq, Ord, Show)
  
class AsDate p f s where
  _Date ::
    Optic' p f s Date

instance AsDate p f Date where
  _Date =
    id

instance (Choice p, Applicative f) => AsDate p f String where
  _Date =
    prism'
      (\(Date d01 d02 d03 d04 d05 d06 d07 d08) -> (digitC #) <$> [d01, d02, d03, d04, d05, d06, d07, d08])
      (\s -> case s of
               [c01, c02, c03, c04, c05, c06, c07, c08] ->
                 let f = (^? digitC)
                 in Date <$>
                      f c01 <*>
                      f c02 <*>
                      f c03 <*>
                      f c04 <*>
                      f c05 <*>
                      f c06 <*>
                      f c07 <*>
                      f c08
               _ ->
                 Nothing)

-- |
-- 
-- >>> parse date "test" "20140508"
-- Right (Date 2 0 1 4 0 5 0 8)
-- 
-- >>> parse date "test" "20140508abc"
-- Right (Date 2 0 1 4 0 5 0 8)
-- 
-- >>> parse date "test" "201405"
-- Left "test" (line 1, column 7):
-- unexpected end of input
-- expecting digit
-- 
-- >>> parse date "test" "201405a9"
-- Left "test" (line 1, column 8):
-- not a digit: a
-- 
-- >>> parse date "test" ""
-- Left "test" (line 1, column 1):
-- unexpected end of input
-- expecting date
date ::
  (Monad f, CharParsing f) =>
  f Date
date =
  Date <$>
    digitCharacter <*>
    digitCharacter <*>
    digitCharacter <*>
    digitCharacter <*>
    digitCharacter <*>
    digitCharacter <*>
    digitCharacter <*>
    digitCharacter <?> "date"
