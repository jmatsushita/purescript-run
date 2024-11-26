module Run.Writer
  ( Writer(..)
  , WRITER
  , liftWriter
  , liftWriterAt
  , tell
  , tellAt
  , censor
  , censorAt
  , foldWriter
  , foldWriterAt
  , runWriter
  , runWriterAt
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Run (Run)
import Run as Run
import Type.Row (type (+))

data Writer w a = Writer w a

derive instance functorWriter :: Functor (Writer w)

type WRITER w r = (writer :: Writer w | r)

liftWriter :: forall w a r. Writer w a -> Run (WRITER w + r) a
liftWriter = liftWriterAt @"writer"

liftWriterAt
  :: forall w a r t @s
   . IsSymbol s
  => Row.Cons s (Writer w) t r
  => Writer w a
  -> Run r a
liftWriterAt = Run.lift @s

tell :: forall w r. w -> Run (writer :: Writer w | r) Unit
tell = tellAt @"writer"

tellAt
  :: forall w r t @s
   . IsSymbol s
  => Row.Cons s (Writer w) t r
  => w
  -> Run r Unit
tellAt w = liftWriterAt @s (Writer w unit)

censor :: forall w a r. (w -> w) -> Run (writer :: Writer w | r) a -> Run (writer :: Writer w | r) a
censor = censorAt @"writer"

censorAt
  :: forall w a r t @s
   . IsSymbol s
  => Row.Cons s (Writer w) t r
  => (w -> w)
  -> Run r a
  -> Run r a
censorAt = loop
  where
  handle = Run.on @s Left Right
  loop f r = case Run.peel r of
    Left a -> case handle a of
      Left (Writer w n) -> do
        tellAt @s (f w)
        loop f n
      Right _ ->
        Run.send a >>= loop f
    Right a ->
      pure a

foldWriter :: forall w b a r. (b -> w -> b) -> b -> Run (WRITER w + r) a -> Run r (Tuple b a)
foldWriter = foldWriterAt @"writer"

foldWriterAt
  :: forall w b a r t @s
   . IsSymbol s
  => Row.Cons s (Writer w) t r
  => (b -> w -> b)
  -> b
  -> Run r a
  -> Run t (Tuple b a)
foldWriterAt = loop
  where
  handle = Run.on @s Left Right
  loop k w r = case Run.peel r of
    Left a -> case handle a of
      Left (Writer w' n) ->
        loop k (k w w') n
      Right a' ->
        Run.send a' >>= foldWriterAt @s k w
    Right a ->
      pure (Tuple w a)

runWriter :: forall w a r. Monoid w => Run (WRITER w + r) a -> Run r (Tuple w a)
runWriter = runWriterAt @"writer"

runWriterAt
  :: forall w a r t @s
   . IsSymbol s
  => Monoid w
  => Row.Cons s (Writer w) t r
  => Run r a
  -> Run t (Tuple w a)
runWriterAt = foldWriterAt @s (<>) mempty
