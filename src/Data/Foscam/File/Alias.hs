{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Data.Foscam.File.Alias(
  Alias(..)
, AsAlias(..)
, AsAliasHead(..)
, AsAliasTail(..)
, alias
) where

import Control.Applicative(Applicative((<*>)), (<$>))
import Control.Category(Category(id))
import Control.Lens(Optic', Choice, Cons(_Cons), prism', lens, (^?), (#))
import Control.Monad(Monad)
import Data.Eq(Eq)
import Data.Foscam.File.AliasCharacter(AliasCharacter, AsAliasCharacter(_AliasCharacter), aliasCharacter)
import Data.Functor(Functor)
import Data.Maybe(Maybe(Nothing, Just))
import Data.Ord(Ord)
import Data.String(String)
import Data.Traversable(traverse)
import Prelude(Show)
import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators(many, (<?>), try)

-- $setup
-- >>> import Text.Parsec

data Alias =
  Alias
    AliasCharacter
    [AliasCharacter]
  deriving (Eq, Ord, Show)

class AsAlias p f s where
  _Alias ::
    Optic' p f s Alias

instance AsAlias p f Alias where
  _Alias =
    id

instance (Choice p, Applicative f) => AsAlias p f String where
  _Alias =
    prism'
      (\(Alias h t) -> (_AliasCharacter #) <$> (h:t))
      (\s -> case s of 
               [] -> 
                 Nothing
               h:t ->
                 let ch = (^? _AliasCharacter)
                 in Alias <$> ch h <*> traverse ch t)

instance Cons Alias Alias AliasCharacter AliasCharacter where
  _Cons = 
    prism'
      (\(h, Alias h' t) -> Alias h (h':t))
      (\(Alias h t) -> case t of 
                         [] ->
                           Nothing
                         u:v ->
                           Just (h, Alias u v))

class AsAliasHead p f s where
  _AliasHead ::
    Optic' p f s AliasCharacter

instance AsAliasHead p f AliasCharacter where
  _AliasHead =
    id

instance (p ~ (->), Functor f) => AsAliasHead p f Alias where
  _AliasHead =
    lens
      (\(Alias h _) -> h)
      (\(Alias _ t) h -> Alias h t)

class AsAliasTail p f s where
  _AliasTail ::
    Optic' p f s [AliasCharacter]

instance AsAliasTail p f [AliasCharacter] where
  _AliasTail =
    id

instance (p ~ (->), Functor f) => AsAliasTail p f Alias where
  _AliasTail =
    lens
      (\(Alias _ t) -> t)
      (\(Alias h _) t -> Alias h t)

-- |
--
-- >>> parse alias "test" "abcdef"
-- Right (Alias (AliasCharacter 'a') [AliasCharacter 'b',AliasCharacter 'c',AliasCharacter 'd',AliasCharacter 'e',AliasCharacter 'f'])
-- 
-- >>> parse alias "test" "abc123"
-- Right (Alias (AliasCharacter 'a') [AliasCharacter 'b',AliasCharacter 'c',AliasCharacter '1',AliasCharacter '2',AliasCharacter '3'])
-- 
-- >>> parse alias "test" "abc*123"
-- Right (Alias (AliasCharacter 'a') [AliasCharacter 'b',AliasCharacter 'c'])
-- 
-- >>> parse alias "test" "abc*"
-- Right (Alias (AliasCharacter 'a') [AliasCharacter 'b',AliasCharacter 'c'])
-- 
-- >>> parse alias "test" ""
-- Left "test" (line 1, column 1):
-- unexpected end of input
-- expecting alias
alias ::
  (Monad f, CharParsing f) =>
  f Alias
alias =
  let a = try aliasCharacter
  in Alias <$> a <*> many a <?> "alias"
