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

bilift :: (x -> y) -> (Reverse y -> Reverse x) -> Monocle x y
bilift f g = Monocle (iso f (\k -> g . k . f))

--first :: Monocle x y -> Monocle (x, z) (y, z)
--first (Monocle l) = Monocle (\k (x, z) -> fmap undefined (l (fmap (\f -> fst . f . (, z)) . k . (, z)) x))

instance Arrow Monocle where
  arr f = Monocle (iso f (\k -> unsafeCoerce . k . f))
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

foo :: Monocle ((PDMove, PDMove) <| Bool) (() <| ())
foo = proc profile -> do
  a <- argmax Player1 [Cooperate,Defect]  -< fmap fst profile
  b <- argmax Player2 [Cooperate,Defect]  -< fmap snd profile
  u <- lift pdPayoffs -< pair (a, b)
  costate -< u
{-}
foo_zero :: Monocle ((PDMove, PDMove) <| Bool) (() <| ())
foo_zero = line1
       >>> line2
       >>> line3
       >>> line4
       >>> line5
       >>> line6
       >>> line7
       >>> line8
       >>> line9
       >>> line10

line1 :: Monocle ((PDMove, PDMove) <| Bool)
                (((PDMove, PDMove) <| Bool), ((PDMove, PDMove) <| Bool))
--                ((PDMove, PDMove), (PDMove, PDMove)) <| Bool
line1 = lift (\profile -> (profile, profile))

line2 :: Monocle (((PDMove, PDMove) <| Bool), ((PDMove, PDMove) <| Bool))
                 (PDMove <| Bool, ((PDMove, PDMove) <| Bool))
--               (PDMove, (PDMove, PDMove)) <| Bool
line2 = first (lift (fmap fst))

line3 :: Monocle (PDMove <| Bool, ((PDMove, PDMove) <| Bool))
                 (PDMove <| Payoffs, ((PDMove, PDMove) <| Bool))
--               (PDMove, (PDMove, PDMove)) <| Payoffs
line3 = first (argmax Player1 [Cooperate, Defect])

line4 :: Monocle (PDMove <| Payoffs, ((PDMove, PDMove) <| Bool))
                 ((PDMove, PDMove) <| Bool, ((PDMove, PDMove) <| Bool, PDMove <| Payoffs))
--               ((PDMove, PDMove), (PDMove, PDMove), PDMove) <| Bool
line4 = arr (\(a, profile) -> (profile, (profile, a)))

line5 :: Monocle ((PDMove, PDMove) <| Bool, ((PDMove, PDMove) <| Bool, PDMove <| Payoffs))
                 (PDMove <| Bool, ((PDMove, PDMove) <| Bool, PDMove <| Payoffs))
--               (PDMove, (PDMove, PDMove), PDMove) <| Bool
line5 = first (lift (fmap snd))

line6 :: Monocle (PDMove <| Bool, ((PDMove, PDMove) <| Bool, PDMove <| Payoffs))
                 (PDMove <| Payoffs, (((PDMove, PDMove) <| Bool), PDMove <| Payoffs))
--               (PDMove, (PDMove, PDMove), PDMove) <| Payoffs
line6 = first (argmax Player2 [Cooperate, Defect])

line7 :: Monocle (PDMove <| Payoffs, (((PDMove, PDMove) <| Bool), PDMove <| Payoffs))
                 (PDMove <| Payoffs, PDMove <| Payoffs)
--               (PDMove, PDMove) <| Payoffs
line7 = lift (\(b, (profile, a)) -> (a, b))

line8 :: Monocle (PDMove <| Payoffs, PDMove <| Payoffs)
                 ((PDMove, PDMove) <| Payoffs)
line8 = lift pair

line9 :: Monocle ((PDMove, PDMove) <| Payoffs)
                 (Payoffs <| Payoffs)
line9 = lift pdPayoffs

line10 :: Monocle (Payoffs <| Payoffs)
                  (() <| ())
line10 = costate
-}
