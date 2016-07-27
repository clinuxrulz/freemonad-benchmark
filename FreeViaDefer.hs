{-# LANGUAGE RankNTypes #-}
module FreeViaDefer where

import Base
import Control.Monad
import Control.Applicative
import qualified Control.Monad.State.Strict as MTL
import Data.Tuple (swap)

newtype Free f a = Free (forall m. (Monad m) => (forall b. f b -> m b) -> m a)

runFree :: forall f m a. (Monad m) => Free f a -> (forall b. f b -> m b) -> m a
runFree (Free k) impl = k impl

liftF :: forall f a. f a -> Free f a
liftF f = Free (\impl -> impl f)

instance Functor (Free f) where
  fmap f (Free k) = Free (\impl -> fmap f (k impl))
  {-# INLINE fmap #-}

instance Applicative (Free f) where
  pure a = Free (\_ -> pure a)
  {-# INLINE pure #-}
  (Free kf) <*> (Free ka) = Free (\impl -> (kf impl) <*> (ka impl))

instance Monad (Free f) where
  return a = Free (\_ -> pure a)
  {-# INLINE return #-}
  (Free k) >>= f = Free (\impl -> (k impl) >>= (\a -> runFree (f a) impl))

instance MonadFree f (Free f) where
  wrap f = join (liftF f)

run :: Free F a -> Int -> (Int, a)
run (Free k) s = swap $ MTL.runState (k impl) s
  where
    impl :: forall a. F a -> MTL.State Int a
    impl f = MTL.state (\s -> swap $ (unF f) s)
