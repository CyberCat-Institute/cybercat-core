{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Cybercatv2 where

import Prelude hiding (id, (.))
import qualified Prelude
import Control.Lens hiding (para)
import Control.Category
import Control.Arrow

import Unsafe.Coerce

import Cybercat (Player(..), PDMove(..))
import qualified Cybercat (Payoffs(..), pdPayoffs)

data (|>) r x = Forward {runForward :: x}

instance Functor ((|>) r) where
  fmap f = Forward . f . runForward

type (<|) x r = r |> x

forward :: x <| s -> x <| r
forward x = Forward (runForward x)

pair :: (x <| r, y <| r) -> (x, y) <| r
pair (x, y) = Forward (runForward x, runForward y)

type family Reverse x

type instance Reverse (x <| r) = r

--type instance Reverse (x, y) = Reverse x
type instance Reverse (x, y) = (Reverse x, Reverse y)

data Monocle x y = Monocle (Lens x (x -> Reverse x) y (y -> Reverse y))

instance Category Monocle where
  id = Monocle Prelude.id
  (Monocle l) . (Monocle m) = Monocle (m Prelude.. l)

lift :: (Reverse x ~ Reverse y) => (x -> y) -> Monocle x y
lift f = Monocle (iso f (. f))

instance Arrow Monocle where
  arr f = Monocle (iso f (\k -> unsafeCoerce . k . f))
  -- this loops the typechecker in ghc 9.0.2
  first (Monocle l) = Monocle (\k (x, z) -> fmap (. fst) (l (\c -> fmap (fst .) _)))
  --first (Monocle l) = Monocle (\k (x, z) -> fmap (. fst) (l (fmap (. (, z)) . k . (, z)) x))
  --first (Monocle l) = Monocle (\k (x, z) -> fmap (. fst) (l (\c -> fmap (fst .) (fmap (. (, z)) (k (c, z)))) x))
  --first (Monocle l) = Monocle (\k (x, z) -> fmap (. fst) (l (fmap (\f c -> fst (f (c, z))) . k . (, z)) x))

costate :: Monocle (x <| x) (() <| ())
costate = Monocle (iso (const (Forward ())) (const runForward))

type Payoffs = Player -> Double

argmax :: Player -> [x] -> Monocle (x <| Bool) (x <| Payoffs)
argmax name xs = Monocle (iso forward (\p x -> all (\x' -> p (forward x) name >= p (Forward x') name) xs))

pdPayoffs :: (PDMove, PDMove) <| Payoffs -> Payoffs <| Payoffs
pdPayoffs = Forward . u . runForward
  where Cybercat.Payoffs u = Cybercat.pdPayoffs
{-}
foo :: Monocle ((PDMove, PDMove) <| Bool) (() <| ())
foo = proc profile -> do
  a <- argmax Player1 [Cooperate,Defect]  -< fmap fst profile
  b <- argmax Player2 [Cooperate,Defect]  -< fmap snd profile
  u <- lift pdPayoffs -< pair (a, b)
  costate -< u

foo_zero :: Monocle ((PDMove, PDMove) <| Bool) (() <| ())
foo_zero = lift (\profile -> (profile, profile))
       >>> first (lift (fmap fst))
       >>> first (argmax Player1 [Cooperate,Defect])
       >>> arr (\(a, profile) -> (profile, (profile, a)))
       >>> first (lift (fmap snd))
       >>> first (argmax Player2 [Cooperate,Defect])
       >>> lift (\(b, (profile, a)) -> (a, b))
       >>> lift pair
       >>> lift pdPayoffs
       >>> costate
-}
