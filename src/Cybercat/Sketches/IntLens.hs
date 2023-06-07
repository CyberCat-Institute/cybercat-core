{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Cybercat.Sketches.IntLens where

import Prelude hiding (Int)
import Data.Functor.Identity
import Control.Monad.Writer
import Control.Monad.State.Lazy
import Control.Lens

type ConcreteInt s t a b = (s, b) -> (a, t)

(>>>) :: ConcreteInt s t a b -> ConcreteInt a b p q -> ConcreteInt s t p q
(f >>> g) (s, q) = let (a, t) = f (s, b)
                       (p, b) = g (a, q)
                    in (p, t)

class (Functor f) => Copointed f where
    extract :: f a -> a

instance Copointed Identity where
    extract = runIdentity

instance Copointed (Writer s) where
    extract = fst . runWriter

instance Copointed (State s) where
    extract f = let (a, s) = runState f s in a

type Int s t a b = forall f. (Copointed f) => LensLike f s t a b

intGet :: Int s t a b -> s -> b -> a
intGet f s b = let (_, a) = runState (f (state . const . (b,)) s) a 
                in a

intPut :: Int s t a b -> s -> b -> t
intPut f s b = runIdentity (f (const (Identity b)) s)

int :: (s -> b -> a) -> (s -> b -> t) -> Int s t a b
int f g k s = let fb = k (f s (extract fb))
               in fmap (g s) fb

g1 (x, y, z) _ = (x, y)
p1 (x, y, z) (x', y') = (x', y', z)
g2 (x, y) _ = x
p2 (x, y) x' = (x', y)
