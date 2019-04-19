{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE PolyKinds  #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A version of 'MockT' with a stateless 'MonadTransControl' instance
module Control.Monad.Mock.Stateless
  (
  -- * The MonadMock class
    MonadMock(..)

  -- * The MockT monad transformer
  , MockT
  , Mock
  , runMockT
  , runMock
  , MockT_

  -- * Actions and actions with results
  , Action(..)
  , WithResult(..)
  , withResultHmap
  ) where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadThrow, MonadMask)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Except (MonadError)
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.Reader (ReaderT(..), MonadReader(..))
import Control.Monad.State (MonadState)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Trans.Control
import Control.Monad.Writer (MonadWriter)
import Data.Primitive.MutVar (MutVar, newMutVar, readMutVar, writeMutVar)
import Data.Type.Equality ((:~:)(..))

import Control.Monad.Mock (Action(..), MonadMock(..))

type MockT f m = MockT_ (PrimState m) m f m

type Mock s f = MockT f (ST s)

-- | Represents both an expected call (an 'Action') and its expected result.
data WithResult f m where
  -- | Matches a specific command
  (:->)     :: f r -> m r -> WithResult f m
  -- | Skips commands as long as the predicate returns something
  SkipWhile :: (forall r. f r -> Maybe (m r)) -> WithResult f m

withResultHmap :: (forall a . m a -> n a) -> WithResult f m -> WithResult f n
withResultHmap f (a :-> mb) = a :-> f mb
withResultHmap f (SkipWhile cond) = SkipWhile (fmap f . cond)

newtype MockT_ s n f m a = MockT (ReaderT (MutVar s [WithResult f n]) m a)
  deriving ( Functor, Applicative, Monad, MonadIO, MonadFix
           , MonadState st, MonadCont, MonadError e, MonadWriter w
           , MonadCatch, MonadThrow, MonadMask
           , MonadTrans, MonadTransControl
           , MonadBase b, MonadBaseControl b
           , PrimMonad)

instance MonadReader r m => MonadReader r (MockT_ s n f m) where
  ask = lift ask
  local f (MockT act) = MockT $ do
    env <- ask
    lift $ local f $ runReaderT act env

runMockT :: forall f m a .
            (Action f, PrimMonad m) =>
            [WithResult f m] -> MockT f m a -> m a
runMockT actions (MockT x) = do
  ref <- newMutVar actions
  r <- runReaderT x ref
  leftovers <- readMutVar ref
  case leftovers of
    [] -> return r
    remainingActions -> error'
      $ "runMockT: expected the following unexecuted actions to be run:\n"
      ++ unlines (map (\(action :-> _) -> "  " ++ showAction action) remainingActions)

runMock :: forall f a. Action f => [WithResult f Identity] -> (forall s. Mock s f a) -> a
runMock actions x = runST $ runMockT (map (\(a :-> b) -> a :-> return(runIdentity b)) actions) x

instance (PrimMonad m, PrimState m ~ s) => MonadMock f (MockT_ s m f m) where
  mockAction fnName action = do
    ref <- MockT ask
    results <- lift $ readMutVar ref
    case results of
      [] -> error'
        $ "runMockT: expected end of program, called " ++ fnName ++ "\n"
        ++ "  given action: " ++ showAction action ++ "\n"
      SkipWhile f : actions
        | Just res <- f action
        -> lift res
        | otherwise -> do
            lift $ writeMutVar ref actions
            mockAction fnName action
      (action' :-> r) : actions
        | Just Refl <- action `eqAction` action' -> do
            lift $ writeMutVar ref actions
            lift r
        | otherwise -> error'
            $ "runMockT: argument mismatch in " ++ fnName ++ "\n"
            ++ "  given: " ++ showAction action ++ "\n"
            ++ "  expected: " ++ showAction action' ++ "\n"


error' :: String -> a
#if MIN_VERSION_base(4,9,0)
error' = errorWithoutStackTrace
#else
error' = error
#endif
