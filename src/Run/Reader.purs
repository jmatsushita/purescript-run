module Run.Reader
  ( Reader(..)
  , READER
  , liftReader
  , liftReaderAt
  , ask
  , asks
  , askAt
  , asksAt
  , local
  , localAt
  , runReader
  , runReaderAt
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Run (Run)
import Run as Run
import Type.Row (type (+))

newtype Reader e a = Reader (e -> a)

derive newtype instance functorReader :: Functor (Reader e)

type READER e r = (reader :: Reader e | r)

liftReader :: forall e a r. Reader e a -> Run (READER e + r) a
liftReader = liftReaderAt @"reader"

liftReaderAt
  :: forall t e a r @s
   . IsSymbol s
  => Row.Cons s (Reader e) t r
  => Reader e a
  -> Run r a
liftReaderAt = Run.lift @s

ask :: forall e r. Run (READER e + r) e
ask = askAt @"reader"

askAt
  :: forall t e r @s
   . IsSymbol s
  => Row.Cons s (Reader e) t r
  => Run r e
askAt = asksAt @s identity

asks :: forall e r a. (e -> a) -> Run (READER e + r) a
asks = asksAt @"reader"

asksAt
  :: forall t e r @s a
   . IsSymbol s
  => Row.Cons s (Reader e) t r
  => (e -> a)
  -> Run r a
asksAt f = liftReaderAt @s (Reader f)

local :: forall e a r. (e -> e) -> Run (READER e + r) a -> Run (READER e + r) a
local = localAt @"reader"

localAt
  :: forall t e a r @s
   . IsSymbol s
  => Row.Cons s (Reader e) t r
  => (e -> e)
  -> Run r a
  -> Run r a
localAt = \f r -> map f (askAt @s) >>= flip runLocal r
  where
  handle = Run.on @s Left Right
  runLocal = loop
    where
    loop e r = case Run.peel r of
      Left a -> case handle a of
        Left (Reader k) ->
          loop e (k e)
        Right _ ->
          Run.send a >>= runLocal e
      Right a ->
        pure a

runReader :: forall e a r. e -> Run (READER e + r) a -> Run r a
runReader = runReaderAt @"reader"

runReaderAt
  :: forall t e a r @s
   . IsSymbol s
  => Row.Cons s (Reader e) t r
  => e
  -> Run r a
  -> Run t a
runReaderAt = loop
  where
  handle = Run.on @s Left Right
  loop e r = case Run.peel r of
    Left a -> case handle a of
      Left (Reader k) ->
        loop e (k e)
      Right a' ->
        Run.send a' >>= runReaderAt @s e
    Right a ->
      pure a
