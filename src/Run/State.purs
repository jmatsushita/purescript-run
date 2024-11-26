module Run.State
  ( State(..)
  , STATE
  , liftState
  , liftStateAt
  , modify
  , modifyAt
  , put
  , putAt
  , get
  , getAt
  , gets
  , getsAt
  , runState
  , runStateAt
  , evalState
  , evalStateAt
  , execState
  , execStateAt
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Prim.Row as Row
import Run (Run)
import Run as Run
import Type.Row (type (+))

data State s a = State (s -> s) (s -> a)

derive instance functorState :: Functor (State s)

type STATE s r = (state :: State s | r)

liftState :: forall s a r. State s a -> Run (STATE s + r) a
liftState = liftStateAt @"state"

liftStateAt
  :: forall q @sym s a r
   . IsSymbol sym
  => Row.Cons sym (State s) q r
  => State s a
  -> Run r a
liftStateAt = Run.lift @sym

modify :: forall s r. (s -> s) -> Run (STATE s + r) Unit
modify = modifyAt @"state"

modifyAt
  :: forall q @sym s r
   . IsSymbol sym
  => Row.Cons sym (State s) q r
  => (s -> s)
  -> Run r Unit
modifyAt f = liftStateAt @sym $ State f (const unit)

put :: forall s r. s -> Run (STATE s + r) Unit
put = putAt @"state"

putAt
  :: forall q @sym s r
   . IsSymbol sym
  => Row.Cons sym (State s) q r
  => s
  -> Run r Unit
putAt = modifyAt @sym <<< const

get :: forall s r. Run (STATE s + r) s
get = getAt @"state"

getAt
  :: forall q @sym s r
   . IsSymbol sym
  => Row.Cons sym (State s) q r
  => Run r s
getAt = liftStateAt @sym $ State identity identity

gets :: forall s t r. (s -> t) -> Run (STATE s + r) t
gets = getsAt @"state"

getsAt
  :: forall q @sym s t r
   . IsSymbol sym
  => Row.Cons sym (State s) q r
  => (s -> t)
  -> Run r t
getsAt = flip map (getAt @sym)

runState :: forall s r a. s -> Run (STATE s + r) a -> Run r (Tuple s a)
runState = runStateAt @"state"

runStateAt
  :: forall q @sym s r a
   . IsSymbol sym
  => Row.Cons sym (State s) q r
  => s
  -> Run r a
  -> Run q (Tuple s a)
runStateAt = loop
  where
  handle = Run.on @sym Left Right
  loop s r = case Run.peel r of
    Left a -> case handle a of
      Left (State t k) ->
        let
          s' = t s
        in
          loop s' (k s')
      Right a' ->
        Run.send a' >>= runStateAt @sym s
    Right a ->
      pure (Tuple s a)

evalState :: forall s r a. s -> Run (STATE s + r) a -> Run r a
evalState = evalStateAt @"state"

evalStateAt
  :: forall q @sym s r a
   . IsSymbol sym
  => Row.Cons sym (State s) q r
  => s
  -> Run r a
  -> Run q a
evalStateAt s = map snd <<< runStateAt @sym s

execState :: forall s r a. s -> Run (STATE s + r) a -> Run r s
execState = execStateAt @"state"

execStateAt
  :: forall q @sym s r a
   . IsSymbol sym
  => Row.Cons sym (State s) q r
  => s
  -> Run r a
  -> Run q s
execStateAt s = map fst <<< runStateAt @sym s
