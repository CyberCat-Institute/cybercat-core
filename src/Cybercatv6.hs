{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}

module Cybercatv6 where

import Cybercatv4 (Bang(..), Player(..), PD(..), pdPayoffs)
import Cybercatv5 ()

data X x = X

type family Reverse x where
  Reverse (X x) = x
  Reverse (Bang x) = x -> Reverse x
  Reverse (x, y) = (Reverse x, Reverse y)
  Reverse x = ()

type Value r x = (x, Reverse x -> r)

unpair :: Value r (x, y) -> (Value (Reverse y -> r) x, Value (Reverse y) y)
unpair ((x, y), k) = ((x, curry k), (y, id))

(#) :: Value (r -> s) (Bang x) -> Value r (Bang y) -> Value s (Bang (x, y))
(Bang x, k1) # (Bang y, k2) = (Bang (x, y), \f -> k1 (fst . f . (, y)) (k2 (snd . f . (x, ))))

type (~>) x y = x -> Value (Reverse x) y

(<~) :: (x ~> y) -> Value r x -> Value r y
f <~ (x, k) = let (y, u) = f x
               in (y, k . u)

lift :: (x -> y) -> (Reverse y -> Reverse x) -> x ~> y
lift f g x = (f x, g)

banglift :: (x -> y) -> (Reverse y -> Reverse x) -> Bang x ~> Bang y
banglift f g (Bang x) = (Bang (f x), \k -> g . k . f)

laxator :: (Bang x, Bang y) ~> Bang (x, y)
laxator (Bang x, Bang y) = (Bang (x, y), \f -> (fst . f . (, y), snd . f . (x, )))

target :: (Reverse x ~ ()) => Bang (x, X x) ~> ()
target = const ((), const (\(x, X) -> ((), x)))

argmax :: (Reverse x ~ ()) => Player -> [x] -> (x, X Bool) ~> Bang (x, X (Player -> Double))
argmax p xs (x, X) = (Bang (x, X), \f -> ((), all (\x' -> snd (f (x, X)) p >= snd (f (x', X)) p) xs))

banglift2 :: (Reverse x ~ (), Reverse y ~ (), Reverse z ~ ())
           => (x -> y -> z) -> Bang ((x, X r), (y, X r)) ~> Bang (z, X r)
banglift2 f = banglift (\((x, X), (y, X)) -> (f x y, X)) (\((), r) -> (((), r), ((), r)))

reverse2 :: (Reverse x ~ (), Reverse y ~ ()) => (p -> q -> r) -> ((x, y), X r) ~> ((x, X p), (y, X q))
reverse2 f = lift (\((x, y), X) -> ((x, X), (y, X))) (\(((), r1), ((), r2)) -> (((), ()), f r1 r2))

pd :: ((PD, PD), X Bool) ~> ()
pd strategies = let (a', b') = unpair (reverse2 (&&) strategies)
                    x = argmax Player1 [Cooperate,Defect] <~ a'
                    y = argmax Player2 [Cooperate,Defect] <~ b'
                    u = banglift2 (curry pdPayoffs) <~ (x # y)
                 in target <~ u
