{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Arrows #-}

module Cybercatv3 where

import Prelude hiding (id, (.))
import qualified Prelude (id, (.))

import Control.Category
import Control.Arrow

import Unsafe.Coerce

import Cybercat (Player(..), PDMove(..))
import Cybercatv2 (type (|>)(..), type (<|), forward, pair, Reverse, Payoffs, pdPayoffs)

data ConcreteLens x y = ConcreteLens {
  view :: x -> y,
  update :: x -> Reverse y -> Reverse x
}

instance Category ConcreteLens where
  id = ConcreteLens {
    view = Prelude.id,
    update = flip const
  }
  (ConcreteLens v2 u2) . (ConcreteLens v1 u1) = ConcreteLens {
    view = v2 . v1,
    update = \x z -> u1 x (u2 (v1 x) z)
  }

bilift :: (x -> y) -> (Reverse y -> Reverse x) -> ConcreteLens x y
bilift f g = ConcreteLens {
  view = f,
  update = const g
}

lift :: (Reverse x ~ Reverse y) => (x -> y) -> ConcreteLens x y
lift f = bilift f id

strength :: ConcreteLens x y -> ConcreteLens (x, z) (y, z)
strength (ConcreteLens v u) = ConcreteLens {
  view = \(x, z) -> (v x, z),
  update = \(x, _) (y', z') -> (u x y', z')
}

instance Arrow ConcreteLens where
  arr f = bilift f unsafeCoerce
  first = strength
{-}
para :: ConcreteMonocle x x
para = ConcreteMonocle id (flip const)
-}
costate :: ConcreteLens (x <| x) (() <| ())
costate = ConcreteLens {
  view = const (Forward ()),
  update = const . runForward
}
{-}
reverse :: (r -> s) -> ConcreteMonocle (x <| s) (x <| r)
reverse f = ConcreteMonocle {
  view = forward,
  update = \x k x' -> f (k (forward x))
}
-}
argmax :: Player -> [x] -> ConcreteLens (x <| Bool) (Diegetic (x <| Payoffs))
argmax name xs = ConcreteLens {
  view = Diegetic . forward,
  update = \x p -> all (\x' -> p (forward x) name >= p (Forward x') name) xs
}

foo :: ConcreteLens ((PDMove, PDMove) <| Bool) (() <| ())
foo = proc ab -> do
  backward (uncurry (&&)) -< Forward ()
  x <- argmax Player1 [Cooperate, Defect] -< fmap fst ab
  y <- argmax Player2 [Cooperate, Defect] -< fmap snd ab
  xy <- nash -< (x, y)
  u <- liftDiegetic pdPayoffs -< xy
  costateDiegetic -< u

foo2 :: ConcreteLens (Diegetic (PDMove <| Payoffs), Diegetic (PDMove <| Payoffs)) (() <| ())
foo2 = proc (x, y) -> do
  xy <- nash -< (x, y)
  u <- liftDiegetic pdPayoffs -< xy
  costateDiegetic -< u

backward :: (r -> s) -> ConcreteLens (x <| s) (x <| r)
backward f = ConcreteLens {
  view = forward,
  update = const f
}

data Diegetic x = Diegetic {runDiegetic :: x}

type instance Reverse (Diegetic x) = x -> Reverse x

liftDiegetic :: (Reverse x ~ Reverse y) => (x -> y) -> ConcreteLens (Diegetic x) (Diegetic y)
liftDiegetic f = ConcreteLens {
  view = Diegetic . f . runDiegetic,
  update = const (. f)
}

costateDiegetic :: ConcreteLens (Diegetic (x <| x)) (() <| ())
costateDiegetic = ConcreteLens {
  view = const (Forward ()),
  update = const (\() -> runForward)
}

nash :: ConcreteLens (Diegetic (x <| p), Diegetic (y <| p)) (Diegetic ((x, y) <| p))
nash = ConcreteLens {
  view = \(x, y) -> Diegetic (pair (runDiegetic x, runDiegetic y)),
  update = \(x, y) k -> (k . pair . (, runDiegetic y), k . pair . (runDiegetic x,))
}

{-}
nash :: ConcreteLens (Diegetic x, Diegetic y) (Diegetic (x, y))
nash = ConcreteLens {
  view = \(x, y) -> Diegetic (runDiegetic x, runDiegetic y),
  update = \(x, y) k -> (fst . k . (, runDiegetic y), snd . k . (runDiegetic x,))
}
-}
