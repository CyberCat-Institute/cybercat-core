{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}

module Cybercat.Core where

import Prelude hiding (id, (.))

import Control.Category

-- A value X of type X x is the "shadow" of a value of type x moving backwards in time
data X x = X

data (<|) r x = Forward {runForward :: x} deriving (Functor)

-- x |> r is isomorphic to (x, X r) when Reverse x ~ (), but more convenient in practice
type (|>) x r = r <| x

-- The linear exponential comonad
data I x = I {runI :: x} deriving (Functor)

type family Reverse x where
  Reverse (x, y) = (Reverse x, Reverse y)
  Reverse (x, y, z) = (Reverse x, Reverse y, Reverse z)
  Reverse (X x) = x
  Reverse (x |> r) = r
  Reverse (I x) = x -> Reverse x
  Reverse (x ~~> y) = (x, Reverse y)
  Reverse x = ()

-- A forward value equipped with a backwards continuation
data (:>) x r = (:>) {runValue :: x, continueValue :: Reverse x -> r}

unpair :: (x, y) :> r -> (x :> (Reverse y -> r), y :> Reverse y)
unpair ((x, y) :> k) = (x :> curry k, y :> id)

unpair' :: ((x, y) |> (r, s)) :> t -> ((x |> r) :> (s -> t), (y |> s) :> s)
unpair' (Forward (x, y) :> k) = (Forward x :> (curry k), Forward y :> id)

-- A (Lens x (Reverse x) y (Reverse y)) written in linear form (see Cybercat.Lens)
type (~>) x y = x -> y :> Reverse x

lift :: (Reverse x ~ Reverse y) => (x -> y) -> x ~> y
lift f x = f x :> id

lift' :: (x -> y) -> (x |> r) ~> (y |> r)
lift' f (Forward x) = Forward (f x) :> id

liftI :: (Reverse x ~ Reverse y) => (x -> y) -> I x ~> I y
liftI f x = fmap f x :> (. f)

liftI' :: (x -> y) -> I (x |> r) ~> I (y |> r)
liftI' f (I x) = I (fmap f x) :> (. fmap f)

backward :: (r -> s) -> (x |> s) ~> (x |> r)
backward f (Forward x) = Forward x :> f

-- Lax monoidal structure of I (aka Nashator), but hacked to be infix on the forwards pass
(#) :: I (x |> r) :> (s -> t) -> I (y |> r) :> s -> I ((x, y) |> r) :> t
(I (Forward x) :> u) # (I (Forward y) :> v) = I (Forward (x, y))
                                           :> \f -> u (\(Forward x') -> f (Forward (x', y)))
                                                      (v (\(Forward y') -> f (Forward (x, y'))))

-- Set a forwards value to be the backwards optimisation target
target :: I (x |> x) ~> ()
target = const (() :> const runForward)

-- Apply a lens to a value
(<~) :: x ~> y -> x :> r -> y :> r
f <~ (x :> k) = let y :> u = f x
                 in y :> (k . u)

-- We sometimes need to put a lens behind a data constructor to keep Haskell happy
data (~~>) x y = BoxLens {unboxLens :: x ~> y}

instance Category (~~>) where
  id = BoxLens (:> id)
  BoxLens g . BoxLens f = BoxLens (\x -> let y :> u = f x
                                             z :> v = g y
                                          in z :> (u . v))
