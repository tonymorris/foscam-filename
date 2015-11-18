{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Data.Foscam.File.DeviceIdCharacter(
  DeviceIdCharacter
, AsDeviceIdCharacter(..)
, deviceIdCharacter
, getDeviceIdCharacter
) where

import Control.Applicative(Applicative(pure))
import Control.Category(id, (.))
import Control.Lens(Optic', Choice, Getter, prism', re)
import Control.Monad(Monad(fail))
import Data.Char(Char)
import Data.Eq(Eq)
import Data.Functor(fmap)
import Data.Foscam.File.Internal(boolj, charP)
import Data.List(elem, (++))
import Data.Ord(Ord)
import Data.String(String)
import Data.Traversable(traverse)
import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>))
import Prelude(Show)

-- $setup
-- >>> import Text.Parsec

newtype DeviceIdCharacter = 
  DeviceIdCharacter Char
  deriving (Eq, Ord, Show)

class AsDeviceIdCharacter p f s where
  _DeviceIdCharacter ::
    Optic' p f s DeviceIdCharacter

instance AsDeviceIdCharacter p f DeviceIdCharacter where
  _DeviceIdCharacter =
    id

instance (Choice p, Applicative f) => AsDeviceIdCharacter p f Char where
  _DeviceIdCharacter =
    prism'
      (\(DeviceIdCharacter c) -> c)
      (fmap DeviceIdCharacter . boolj (`elem` (['A'..'F'] ++ ['0'..'9'])))

instance (p ~ (->), Applicative f) => AsDeviceIdCharacter p f String where
  _DeviceIdCharacter =
    traverse . _DeviceIdCharacter

getDeviceIdCharacter ::
  Getter DeviceIdCharacter Char
getDeviceIdCharacter =
  re _DeviceIdCharacter

-- |
--
-- >>> parse deviceIdCharacter "test" "A"
-- Right (DeviceIdCharacter 'A')
-- 
-- >>> parse deviceIdCharacter "test" "0"
-- Right (DeviceIdCharacter '0')
-- 
-- >>> parse deviceIdCharacter "test" "0abc"
-- Right (DeviceIdCharacter '0')
-- 
-- >>> parse deviceIdCharacter "test" "a"
-- Left "test" (line 1, column 2):
-- not a device ID character: a
-- 
-- >>> parse deviceIdCharacter "test" "G"
-- Left "test" (line 1, column 2):
-- not a device ID character: G
--
-- >>> parse deviceIdCharacter "test" ""
-- Left "test" (line 1, column 1):
-- unexpected end of input
-- expecting device ID character    
deviceIdCharacter ::
  (Monad f, CharParsing f) =>
  f DeviceIdCharacter
deviceIdCharacter =
  charP (fail . ("not a device ID character: " ++) . pure) _DeviceIdCharacter <?> "device ID character"
