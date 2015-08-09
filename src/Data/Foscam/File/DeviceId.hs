{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Foscam.File.DeviceId(
  DeviceId(..)
, AsDeviceId(..)
, deviceId
) where

import Control.Applicative(Applicative((<*>)), (<$>))
import Control.Category(id)
import Control.Lens(Optic', Choice, prism', (^?), ( # ))
import Control.Monad(Monad)
import Data.Eq(Eq)
import Data.Foscam.File.DeviceIdCharacter
import Data.Functor(fmap)
import Data.Maybe(Maybe(Nothing))
import Data.Ord(Ord)
import Data.String(String)
import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>))
import Prelude(Show)


-- $setup
-- >>> import Text.Parsec

data DeviceId =
  DeviceId
    DeviceIdCharacter
    DeviceIdCharacter
    DeviceIdCharacter
    DeviceIdCharacter
    DeviceIdCharacter
    DeviceIdCharacter
    DeviceIdCharacter
    DeviceIdCharacter
    DeviceIdCharacter
    DeviceIdCharacter
    DeviceIdCharacter
    DeviceIdCharacter
  deriving (Eq, Ord, Show)

class AsDeviceId p f s where
  _DeviceId ::
    Optic' p f s DeviceId

instance AsDeviceId p f DeviceId where
  _DeviceId =
    id

instance (Choice p, Applicative f) => AsDeviceId p f String where
  _DeviceId =
    prism'
      (\(DeviceId d01 d02 d03 d04 d05 d06 d07 d08 d09 d10 d11 d12) -> fmap (_DeviceIdCharacter #) [d01, d02, d03, d04, d05, d06, d07, d08, d09, d10, d11, d12])
      (\s -> case s of
               [c01, c02, c03, c04, c05, c06, c07, c08, c09, c10, c11, c12] ->
                 let f = (^? _DeviceIdCharacter)
                 in DeviceId <$>
                      f c01 <*>
                      f c02 <*>
                      f c03 <*>
                      f c04 <*>
                      f c05 <*>
                      f c06 <*>
                      f c07 <*>
                      f c08 <*>
                      f c09 <*>
                      f c10 <*>
                      f c11 <*>
                      f c12
               _ ->
                 Nothing)

-- |
--
-- >>> parse deviceId "test" "AB0934233DEF"
-- Right (DeviceId (DeviceIdCharacter 'A') (DeviceIdCharacter 'B') (DeviceIdCharacter '0') (DeviceIdCharacter '9') (DeviceIdCharacter '3') (DeviceIdCharacter '4') (DeviceIdCharacter '2') (DeviceIdCharacter '3') (DeviceIdCharacter '3') (DeviceIdCharacter 'D') (DeviceIdCharacter 'E') (DeviceIdCharacter 'F'))
-- 
-- >>> parse deviceId "test" "AB0934233DEFabc"
-- Right (DeviceId (DeviceIdCharacter 'A') (DeviceIdCharacter 'B') (DeviceIdCharacter '0') (DeviceIdCharacter '9') (DeviceIdCharacter '3') (DeviceIdCharacter '4') (DeviceIdCharacter '2') (DeviceIdCharacter '3') (DeviceIdCharacter '3') (DeviceIdCharacter 'D') (DeviceIdCharacter 'E') (DeviceIdCharacter 'F'))
-- 
-- >>> parse deviceId "test" "AB0934233DX"
-- Left "test" (line 1, column 12):
-- not a device ID character: X
-- 
-- >>> parse deviceId "test" "AB0934233DEf"
-- Left "test" (line 1, column 13):
-- not a device ID character: f
-- 
-- >>> parse deviceId "test" ""
-- Left "test" (line 1, column 1):
-- unexpected end of input
-- expecting device ID
deviceId ::
  (Monad f, CharParsing f) =>
  f DeviceId
deviceId =
  DeviceId <$>
    deviceIdCharacter <*>
    deviceIdCharacter <*>
    deviceIdCharacter <*>
    deviceIdCharacter <*>
    deviceIdCharacter <*>
    deviceIdCharacter <*>
    deviceIdCharacter <*>
    deviceIdCharacter <*>
    deviceIdCharacter <*>
    deviceIdCharacter <*>
    deviceIdCharacter <*>
    deviceIdCharacter <?> "device ID"
