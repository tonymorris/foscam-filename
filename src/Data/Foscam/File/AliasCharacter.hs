{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Foscam.File.AliasCharacter (
  AliasCharacter
, AsAliasCharacter(..)
, aliasCharacter
) where

import Control.Applicative(Applicative(pure), (<$>))
import Control.Category(Category((.), id))
import Control.Lens(Optic', Choice, prism')
import Control.Monad(Monad(fail))
import Data.Char(Char)
import Data.Eq(Eq)
import Data.Foscam.File.Internal(boolj, charP)
import Data.List((++), notElem)
import Data.Ord(Ord)
import Prelude(Show)
import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec

newtype AliasCharacter = 
  AliasCharacter Char
  deriving (Eq, Ord, Show)

class AsAliasCharacter p f s where
  _AliasCharacter ::
    Optic' p f s AliasCharacter

instance AsAliasCharacter p f AliasCharacter where
  _AliasCharacter =
    id

instance (Choice p, Applicative f) => AsAliasCharacter p f Char where
  _AliasCharacter =
    prism'
      (\(AliasCharacter c) -> c)
      ((AliasCharacter <$>) . boolj (`notElem` ['/', ':', '*', '?', '"', '<', '>', '(', ')']))

-- todo: _1 to _12 ??   

-- |
--
-- >>> parse aliasCharacter "test" "2062"
-- Right (AliasCharacter '2')
-- 
-- >>> parse aliasCharacter "test" "2"
-- Right (AliasCharacter '2')
-- 
-- >>> parse aliasCharacter "test" "abc"
-- Right (AliasCharacter 'a')
-- 
-- >>> parse aliasCharacter "test" "*"
-- Left "test" (line 1, column 2):
-- not an alias character: *
-- 
-- >>> parse aliasCharacter "test" "<"
-- Left "test" (line 1, column 2):
-- not an alias character: <
-- 
-- >>> parse aliasCharacter "test" ""
-- Left "test" (line 1, column 1):
-- unexpected end of input
-- expecting alias character
aliasCharacter ::
  (Monad f, CharParsing f) =>
  f AliasCharacter
aliasCharacter =
  charP (fail . ("not an alias character: " ++) . pure) _AliasCharacter <?> "alias character"
  