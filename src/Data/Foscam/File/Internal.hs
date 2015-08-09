{-# LANGUAGE NoImplicitPrelude #-}

module Data.Foscam.File.Internal(
  charP
, digitCharacter
, boolj
) where

import Control.Applicative(pure)
import Control.Category((.))
import Control.Monad(Monad((>>=), fail))
import Data.Bool(bool, Bool)
import Data.Char(Char)
import Data.Digit(Digit, digitC)
import Data.Maybe(Maybe(Nothing, Just), maybe)
import Data.Monoid(First, (<>))
import Control.Lens(Getting, (^?))
import Text.Parser.Char(CharParsing, anyChar)
import Text.Parser.Combinators((<?>))

charP :: 
  (Monad f, CharParsing f) =>
  (Char -> f a)
  -> Getting (First a) Char a
  -> f a
charP fl p =
  anyChar >>= \c -> maybe (fl c) pure (c ^? p) 

digitCharacter ::
  (Monad f, CharParsing f) =>
  f Digit
digitCharacter =
  charP (fail . ("not a digit: " <>) . pure) digitC <?> "digit"

boolj ::
  (a -> Bool)
  -> a
  -> Maybe a
boolj p x = 
  bool Nothing (Just x) (p x)
