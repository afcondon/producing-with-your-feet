module Util.Debounce
  ( Debouncer
  , makeDebouncerRef
  , debounce
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds, delay, forkAff, killFiber)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Ref (Ref)
import Effect.Ref as Ref

type Debouncer = Ref (Maybe (Aff Unit -> Aff Unit))

makeDebouncerRef :: Effect (Ref (Maybe (Aff Unit -> Aff Unit)))
makeDebouncerRef = Ref.new Nothing

debounce :: forall m. MonadAff m => Milliseconds -> Ref (Maybe (Aff Unit -> Aff Unit)) -> Aff Unit -> m Unit
debounce ms ref action = liftAff do
  -- Cancel any pending debounced action
  mCancel <- liftEffect $ Ref.read ref
  case mCancel of
    Just cancel -> cancel (pure unit)
    Nothing -> pure unit
  -- Fork a new delayed action
  fiber <- forkAff do
    delay ms
    action
  -- Store the cancel function
  liftEffect $ Ref.write (Just (\_ -> killFiber (error "debounce") fiber)) ref
