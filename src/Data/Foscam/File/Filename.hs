{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Data.Foscam.File.Filename(
  Filename(..)
, AsFilename(..)
, filename
) where

import Control.Applicative(Applicative((<*>)), (<*), (<$>))
import Control.Category(id)
import Control.Lens(Optic', lens)
import Control.Monad(Monad)
import Data.Digit(Digit)
import Data.Eq(Eq)
import Data.Foscam.File.Internal(digitCharacter)
import Data.Foscam.File.Alias(AsAlias(_Alias), Alias, alias)
import Data.Foscam.File.Date(AsDate(_Date), Date, date)
import Data.Foscam.File.DeviceId(AsDeviceId(_DeviceId), DeviceId, deviceId)
import Data.Foscam.File.ImageId(AsImageId(_ImageId), ImageId, imageId)
import Data.Foscam.File.Time(AsTime(_Time), Time, time)
import Data.Functor(Functor)
import Data.Ord(Ord)
import Text.Parser.Char(CharParsing, char, string)
import Text.Parser.Combinators((<?>))
import Prelude(Show)

-- $setup
-- >>> import Text.Parsec

data Filename =
  Filename
    DeviceId
    Alias
    Digit
    Date
    Time
    ImageId
  deriving (Eq, Ord, Show)

class AsFilename p f s where
  _Filename ::
    Optic' p f s Filename

instance AsFilename p f Filename where
  _Filename =
    id

instance (p ~ (->), Functor f) => AsDeviceId p f Filename where
  _DeviceId =
    lens
      (\(Filename i _ _ _ _ _) -> i)
      (\(Filename _ a x d t m) i -> Filename i a x d t m)

instance (p ~ (->), Functor f) => AsAlias p f Filename where
  _Alias =
    lens
      (\(Filename _ a _ _ _ _) -> a)
      (\(Filename i _ x d t m) a -> Filename i a x d t m)

instance (p ~ (->), Functor f) => AsDate p f Filename where
  _Date =
    lens
      (\(Filename _ _ _ d _ _) -> d)
      (\(Filename i a x _ t m) d -> Filename i a x d t m)

instance (p ~ (->), Functor f) => AsTime p f Filename where
  _Time =
    lens
      (\(Filename _ _ _ _ t _) -> t)
      (\(Filename i a x d _ m) t -> Filename i a x d t m)

instance (p ~ (->), Functor f) => AsImageId p f Filename where
  _ImageId =
    lens
      (\(Filename _ _ _ _ _ m) -> m)
      (\(Filename i a x d t _) m -> Filename i a x d t m)

-- |
--
-- >>> parse filename "test" "00626E44C831(house)_1_20150209134121_2629.jpg"
-- Right (Filename (DeviceId (DeviceIdCharacter '0') (DeviceIdCharacter '0') (DeviceIdCharacter '6') (DeviceIdCharacter '2') (DeviceIdCharacter '6') (DeviceIdCharacter 'E') (DeviceIdCharacter '4') (DeviceIdCharacter '4') (DeviceIdCharacter 'C') (DeviceIdCharacter '8') (DeviceIdCharacter '3') (DeviceIdCharacter '1')) (Alias (AliasCharacter 'h') [AliasCharacter 'o',AliasCharacter 'u',AliasCharacter 's',AliasCharacter 'e']) 1 (Date 2 0 1 5 0 2 0 9) (Time 1 3 4 1 2 1) (ImageId 2 [6,2,9]))
--
-- >>> parse filename "test" "00626E44C829(garage)_1_20140313234556_2660.jpg"
-- Right (Filename (DeviceId (DeviceIdCharacter '0') (DeviceIdCharacter '0') (DeviceIdCharacter '6') (DeviceIdCharacter '2') (DeviceIdCharacter '6') (DeviceIdCharacter 'E') (DeviceIdCharacter '4') (DeviceIdCharacter '4') (DeviceIdCharacter 'C') (DeviceIdCharacter '8') (DeviceIdCharacter '2') (DeviceIdCharacter '9')) (Alias (AliasCharacter 'g') [AliasCharacter 'a',AliasCharacter 'r',AliasCharacter 'a',AliasCharacter 'g',AliasCharacter 'e']) 1 (Date 2 0 1 4 0 3 1 3) (Time 2 3 4 5 5 6) (ImageId 2 [6,6,0]))
--
-- >>> parse filename "test" "00626E44C82x(garage)_1_20140313234556_2660.jpg"
-- Left "test" (line 1, column 13):
-- not a device ID character: x
--
-- >>> parse filename "test" "00626E44C829(gara*ge)_1_20140313234556_2660.jpg"
-- Left "test" (line 1, column 19):
-- not an alias character: *
--
-- >>> parse filename "test" "00626E44C829(garage) 1_20140313234556_2660.jpg"
-- Left "test" (line 1, column 20):
-- unexpected " "
-- expecting ")_"
--
-- >>> parse filename "test" "00626E44C829(garage)_x_20140313234556_2660.jpg"
-- Left "test" (line 1, column 23):
-- not a digit: x
--
-- >>> parse filename "test" "00626E44C829(garage)_1 20140313234556_2660.jpg"
-- Left "test" (line 1, column 23):
-- unexpected " "
-- expecting "_"
--
-- >>> parse filename "test" "00626E44C829(garage)_1_x0140313234556_2660.jpg"
-- Left "test" (line 1, column 25):
-- not a digit: x
filename ::
  (Monad f, CharParsing f) =>
  f Filename
filename =
  Filename <$>
    deviceId <*
    char '(' <*>
    alias <*
    string ")_" <*>
    digitCharacter <*
    char '_' <*>
    date <*>
    time <*
    char '_' <*>
    imageId <*
    string ".jpg" <?> "file"
