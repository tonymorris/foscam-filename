{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Foscam.File.Time(
  Time(..)
, AsTime(..)
, time
) where

import Control.Applicative(Applicative((<*>)), (<$>))
import Control.Category(id)
import Control.Lens(Optic', Choice, prism', (^?), ( # ))
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

data Time =
  Time
    Digit
    Digit
    Digit
    Digit
    Digit
    Digit
  deriving (Eq, Ord, Show)

class AsTime p f s where
  _Time ::
    Optic' p f s Time

instance AsTime p f Time where
  _Time =
    id

instance (Choice p, Applicative f) => AsTime p f String where
  _Time =
    prism'
      (\(Time d01 d02 d03 d04 d05 d06) -> (digitC #) <$> [d01, d02, d03, d04, d05, d06])
      (\s -> case s of
               [c01, c02, c03, c04, c05, c06] ->
                 let f = (^? digitC)
                 in Time <$>
                      f c01 <*>
                      f c02 <*>
                      f c03 <*>
                      f c04 <*>
                      f c05 <*>
                      f c06
               _ ->
                 Nothing)

-- |
--
-- >>> parse time "test" "134122"
-- Right (Time 1 3 4 1 2 2)
--
-- >>> parse time "test" "134122abc"
-- Right (Time 1 3 4 1 2 2)
--
-- >>> parse time "test" "1341"
-- Left "test" (line 1, column 5):
-- unexpected end of input
-- expecting digit
time ::
  (Monad f, CharParsing f) =>
  f Time
time =
  Time <$>
    digitCharacter <*>
    digitCharacter <*>
    digitCharacter <*>
    digitCharacter <*>
    digitCharacter <*>
    digitCharacter <?> "time"
