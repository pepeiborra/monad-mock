{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wredundant-constraints #-}

-- | A version of 'MockT' with a stateless 'MonadTransControl' instance
module Control.Monad.Mock.Stateless
  (
  -- * The MonadMock class
    MonadMock(..)

  -- * The MockT monad transformer
  , MockT
  , Mock
  , runMockT
  , runMockST
  , MockT_

  -- * Actions and actions with results
  , Action(..)
  , WithResult(..)
  , withResultHmap
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadThrow, MonadMask)
import Control.Monad.Cont (MonadCont)
import Control.Monad.Except (MonadError)
import Control.Monad.Fix
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad(..))
import Control.Monad.Reader (ReaderT(..), MonadReader(..))
import Control.Monad.State (MonadState)
import Control.Monad.ST (ST)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Trans.Control
import Control.Monad.Writer (MonadWriter)
import Data.Primitive.MutVar (atomicModifyMutVar, MutVar, newMutVar, readMutVar, writeMutVar)
import Data.Type.Equality ((:~:)(..))

import Control.Monad.Mock (Action(..), MonadMock(..))

type MockT f m = MockT_ (PrimState m) m f m

type Mock s f = MockT f (ST s)

-- | Represents both an expected call (an 'Action') and its expected result.
data WithResult f m where
  -- | Matches a specific command
  (:->)     :: f r -> m r -> WithResult f m
  -- | Matches one of a set of commands
  Match     :: String -> (forall r. f r -> Maybe (m r)) -> WithResult f m
  -- | Skips commands as long as the predicate returns something
  SkipWhile :: String -> (forall r. f r -> Maybe (m r)) -> WithResult f m
  -- | Choice
  Either    :: WithResult f m -> WithResult f m -> WithResult f m
  -- | Trivial failure
  ZeroMatch   :: WithResult f m

instance Action f => Show (WithResult f m) where
  show (action :-> _) = showAction action
  show (Match n _) = n
  show (SkipWhile n _) = n
  show (Either a b) = show a ++ " OR " ++ show b
  show ZeroMatch = "FAIL"

withResultHmap :: (forall a . m a -> n a) -> WithResult f m -> WithResult f n
withResultHmap f (a :-> mb) = a :-> f mb
withResultHmap f (SkipWhile name cond) = SkipWhile name (fmap f . cond)
withResultHmap f (Match name cond) = Match name (fmap f . cond)
withResultHmap f (Either a b) = Either (withResultHmap f a) (withResultHmap f b)
withResultHmap _ ZeroMatch = ZeroMatch

-- | @MockT_ s n f m a@:
--   - @s@ - prim monad state
--   - @n@ - outer monad stack
--   - @f@ - action functor
--   - @m@ - lower monad
--   - @a@ - result type
newtype MockT_ s n f m a = MockT {unMockT :: ReaderT (MutVar s [WithResult f n]) m a}
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
            [WithResult f (MockT f m)] -> MockT f m a -> m a
runMockT actions (MockT x) = do
  ref <- newMutVar []
  let actions' = withResultHmap (flip runReaderT ref . unMockT) <$> actions
  writeMutVar ref actions'
  r <- runReaderT x ref
  leftovers <- readMutVar ref
  case leftovers of
    [] -> return r
    remainingActions -> error'
      $ "runMockT: expected the following unexecuted actions to be run:\n"
      ++ unlines (map (("  " ++) . show) remainingActions)

runMockST :: Action f => [WithResult f (Mock s f)] -> Mock s f a -> ST s a
runMockST = runMockT

instance (PrimMonad m, PrimState m ~ s) => MonadMock f (MockT_ s m f m) where
  mockAction fnName action = do
    ref <- MockT ask
    join $ lift $ atomicModifyMutVar ref $ either error' id . match fnName action

match
  :: (Action f, PrimMonad m)
  => String
  -> f r
  -> [WithResult f m]
  -> Either String ([WithResult f m], MockT_ (PrimState m) m f m r)
match fnName action queue = case queue of
  [] ->
    Left
      $  "runMockT: expected end of program, called "
      ++ fnName
      ++ "\n"
      ++ "  given action: "
      ++ showAction action
      ++ "\n"
  Either a b : actions ->
    match fnName action (a:actions) <|> match fnName action (b:actions)

  ZeroMatch : _actions -> Left "ZeroMatch"

  SkipWhile _ f : actions | Just res <- f action -> Right (queue, lift res)
                          | otherwise -> match fnName action actions
  Match name f : actions
    | Just res <- f action
    -> Right (actions, lift res)
    | otherwise
    -> Left
      $  "runMockT: argument mismatch in "
      ++ fnName
      ++ "\n"
      ++ "  given: "
      ++ showAction action
      ++ "\n"
      ++ "  expected: "
      ++ name
  (action' :-> r) : actions
    | Just Refl <- action `eqAction` action'
    -> Right (actions, lift r)
    | otherwise
    -> Left
      $  "runMockT: argument mismatch in "
      ++ fnName
      ++ "\n"
      ++ "  given: "
      ++ showAction action
      ++ "\n"
      ++ "  expected: "
      ++ showAction action'
      ++ "\n"

error' :: String -> a
#if MIN_VERSION_base(4,9,0)
error' = errorWithoutStackTrace
#else
error' = error
#endif
