module Run.Except
  ( Except(..)
  , EXCEPT
  , FAIL
  , liftExcept
  , liftExceptAt
  , runExcept
  , runExceptAt
  , runFail
  , runFailAt
  , throw
  , throwAt
  , fail
  , failAt
  , rethrow
  , rethrowAt
  , note
  , noteAt
  , fromJust
  , fromJustAt
  , catch
  , catchAt
  ) where

import Prelude

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe')
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Run (Run)
import Run as Run
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

newtype Except :: forall k. Type -> k -> Type
newtype Except e a = Except e

derive instance functorExcept :: Functor (Except e)

type EXCEPT :: forall k. Type -> Row (k -> Type) -> Row (k -> Type)
type EXCEPT e r = (except :: Except e | r)

type Fail :: forall k. k -> Type
type Fail = Except Unit

type FAIL :: forall k. Row (k -> Type) -> Row (k -> Type)
type FAIL r = EXCEPT Unit r

liftExcept :: forall e a r. Except e a -> Run (EXCEPT e + r) a
liftExcept = liftExceptAt @"except"

liftExceptAt
  :: forall t e a r @s
   . IsSymbol s
  => Row.Cons s (Except e) t r
  => Except e a
  -> Run r a
liftExceptAt = Run.lift @s

throw :: forall e a r. e -> Run (EXCEPT e + r) a
throw = throwAt @"except"

throwAt
  :: forall t e a r @s
   . IsSymbol s
  => Row.Cons s (Except e) t r
  => e
  -> Run r a
throwAt = liftExceptAt @s <<< Except

fail :: forall a r. Run (FAIL + r) a
fail = failAt @"except"

failAt
  :: forall t a r @s
   . IsSymbol s
  => Row.Cons s Fail t r
  => Run r a
failAt = throwAt @s unit

rethrow :: forall e a r. Either e a -> Run (EXCEPT e + r) a
rethrow = rethrowAt @"except"

rethrowAt
  :: forall t e a r @s
   . IsSymbol s
  => Row.Cons s (Except e) t r
  => Either e a
  -> Run r a
rethrowAt = either (throwAt @s) pure

note :: forall e a r. e -> Maybe a -> Run (EXCEPT e + r) a
note = noteAt @"except"

noteAt
  :: forall t e a r @s
   . IsSymbol s
  => Row.Cons s (Except e) t r
  => e
  -> Maybe a
  -> Run r a
noteAt e = maybe' (\_ -> throwAt @s e) pure

fromJust :: forall a r. Maybe a -> Run (FAIL + r) a
fromJust = fromJustAt @"except"

fromJustAt
  :: forall t a r @s
   . IsSymbol s
  => Row.Cons s Fail t r
  => Maybe a
  -> Run r a
fromJustAt = noteAt @s unit

catch :: forall e a r. (e -> Run r a) -> Run (EXCEPT e + r) a -> Run r a
catch = catchAt @"except"

catchAt
  :: forall t e a r @s
   . IsSymbol s
  => Row.Cons s (Except e) t r
  => (e -> Run t a)
  -> Run r a
  -> Run t a
catchAt = loop
  where
  handle = Run.on @s Left Right
  loop k r = case Run.peel r of
    Left a -> case handle a of
      Left (Except e) ->
        k e
      Right a' ->
        Run.send a' >>= loop k
    Right a ->
      pure a

runExcept :: forall e a r. Run (EXCEPT e + r) a -> Run r (Either e a)
runExcept = runExceptAt @"except"

runExceptAt
  :: forall t e a r @s
   . IsSymbol s
  => Row.Cons s (Except e) t r
  => Run r a
  -> Run t (Either e a)
runExceptAt = loop
  where
  handle = Run.on @s Left Right
  loop r = case Run.peel r of
    Left a -> case handle a of
      Left (Except e) ->
        pure (Left e)
      Right a' ->
        Run.send a' >>= loop
    Right a ->
      pure (Right a)

runFail :: forall a r. Run (FAIL + r) a -> Run r (Maybe a)
runFail = runFailAt @"except"

runFailAt
  :: forall t a r @s
   . IsSymbol s
  => Row.Cons s Fail t r
  => Run r a
  -> Run t (Maybe a)
runFailAt = map (either (const Nothing) Just) <<< runExceptAt @s
