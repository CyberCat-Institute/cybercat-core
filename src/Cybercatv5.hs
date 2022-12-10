{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

module Cybercatv5 where

import Prelude hiding (reverse, return, (>>=))

import Cybercatv4 (type (|>)(..), type (<|), forward, pair, Reverse, Bang(..), Player(..), PD(..), pdPayoffs)

type Value r x = (x, Reverse x -> r)

type (~>) x y = x -> Value (Reverse x) y

(<~) :: (x ~> y) -> Value r x -> Value r y
f <~ (x, k) = let (y, u) = f x
               in (y, k . u)

embed :: x -> Value (Reverse x) x
embed x = (x, id)

banglax :: (Bang x, Bang y) ~> Bang (x, y)
banglax (Bang x, Bang y) = (Bang (x, y), \f -> (fst . f . (, y), snd . f . (x, )))

unpack :: Value z ((x, y) <| (r, s)) -> (Value (s -> z) (x <| r), Value s (y <| s))
unpack ((Forward (x, y)), k) = ((Forward x, curry k), (Forward y, id))

nashator :: (Value (z' -> z) (Bang (x <| r)), Value z' (Bang (y <| r))) -> Value z (Bang ((x, y) <| r))
nashator ((Bang x, kx), (Bang y, ky)) = (Bang (pair (x, y)), \f -> kx (f . pair . (, y)) (ky (f . pair . (x, ))))

lift :: (x -> y) -> (x <| r) ~> (y <| r)
lift f x = (fmap f x, id)

reverse :: (r -> s) -> (x <| s) ~> (x <| r)
reverse f x = (forward x, f)

banglift :: (x -> y) -> Bang (x <| r) ~> Bang (y <| r)
banglift f (Bang x) = (Bang (fmap f x), (. fmap f))

bangcounit :: Bang (x <| x) ~> ()
bangcounit = const ((), const runForward)

argmax :: Player -> [x] -> (x <| Bool) ~> Bang (x <| (Player -> Double))
argmax p xs x = (Bang (forward x), \f -> all (\x' -> f (forward x) p >= f (Forward x') p) xs)

pd :: ((PD, PD) <| Bool) ~> ()
pd ab = let (a, b) = unpack (reverse (uncurry (&&)) ab)
            x = argmax Player1 [Cooperate,Defect] <~ a
            y = argmax Player2 [Cooperate,Defect] <~ b
            u = banglift pdPayoffs <~ nashator (x, y)
         in bangcounit <~ u
