{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RebindableSyntax #-}

module Cybercatv4 where

import Prelude hiding (reverse, return, (>>=))

import Control.Lens

dup :: x -> (x, x)
dup x = (x, x)

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

type instance Reverse () = ()

type instance Reverse (x, y) = (Reverse x, Reverse y)

--type Lens x y = forall f . (Functor f) => (y -> f (Reverse y)) -> (x -> f (Reverse x))
type (~>) x y = Lens x (Reverse x) y (Reverse y)

bilift :: (x -> y) -> (Reverse y -> Reverse x) -> x ~> y
bilift f g k = fmap g . k . f

lift :: (x -> y) -> (x <| r) ~> (y <| r)
lift f k = k . fmap f

reverse :: (r -> s) -> (x <| s) ~> (x <| r)
reverse = bilift forward

liftPair :: (x <| r, y <| r) ~> ((x, y) <| r)
liftPair k = fmap dup . k . pair

unpack :: ((x, y) <| (s, r)) ~> (x <| s, y <| r)
unpack k (Forward (x, y)) = k (Forward x, Forward y)

strength :: x ~> y -> (x, z) ~> (y, z)
strength l = l `alongside` id

counit :: (x <| x) ~> ()
counit k x = fmap (const (runForward x)) (k ())

data Bang x = Bang {unbang :: x}

instance Functor Bang where
  fmap f (Bang x) = Bang (f x)

type instance Reverse (Bang x) = x -> Reverse x

banglift :: (x -> y) -> Bang (x <| r) ~> Bang (y <| r)
banglift f k = fmap (. fmap f) . k . Bang . fmap f . unbang

--banglift :: (Reverse x ~ Reverse y) => (x -> y) -> Bang x ~> Bang y
--banglift f k = fmap (. f) . k . fmap f

bangpair :: Bang (x <| r, y <| r) ~> Bang ((x, y) <| r)
bangpair k = fmap (\f -> dup . f . pair) . k . fmap pair

bangcounit :: Bang (x <| x) ~> ()
bangcounit k = const (fmap (const runForward) (k ()))

bangmap :: x ~> y -> Bang x ~> Bang y
bangmap l k = fmap const . l (\y -> fmap ($ y) (k (Bang y))) . unbang

bangextract :: Bang x ~> x
bangextract k = fmap const . k . unbang

bangduplicate :: Bang x ~> Bang (Bang x)
bangduplicate k = fmap (\f x -> f (Bang x) x) . k . Bang

banglax :: (Bang x, Bang y) ~> Bang (x, y)
banglax k (Bang x, Bang y) = fmap (\f -> (fst . f . (, y), snd . f . (x,))) (k (Bang (x, y)))

nashator :: (Bang (x <| r), Bang (y <| r)) ~> Bang ((x, y) <| r)
nashator = banglax . bangpair

----- What's going on here, ! should not be a monad?!

return :: x ~> Bang x
return k x = fmap ($ x) (k (Bang x))

bangmult :: Bang (Bang x) ~> Bang x
bangmult k = fmap const . k . unbang
-- NOTE: there's at least 2 other terms of this type
--bangmult k = fmap (\f (Bang x') _ -> f x') . k . unbang
--bangmult k (Bang x) = fmap (\f _ _ -> f (unbang x)) (k x)

data (~~>) x y = MkLens (x ~> y)

type instance Reverse (x ~~> y) = (x, Reverse y)

(>>=) :: Bang x ~> ((x ~~> Bang y) ~~> Bang y)
(>>=) k (Bang x) = fmap (\(MkLens f, g) -> f .~ g)
                           (k (MkLens (\k' (MkLens f) -> fmap (x,) (k' ({-x ^. f-}getConst (f Const x))))))

-------

data Player = Player1 | Player2 deriving (Show)

argmax :: Player -> [x] -> (x <| Bool) ~> Bang (x <| (Player -> Double))
argmax p xs k x = fmap (\f -> all (\x' -> f (forward x) p >= f (Forward x') p) xs) (k (Bang (forward x)))

data PD = Cooperate | Defect deriving (Show)

pdPayoffs :: (PD, PD) -> Player -> Double
pdPayoffs (Cooperate, Cooperate) = \case {Player1 -> 2; Player2 -> 2}
pdPayoffs (Cooperate, Defect)    = \case {Player1 -> 0; Player2 -> 3}
pdPayoffs (Defect,    Cooperate) = \case {Player1 -> 3; Player2 -> 0}
pdPayoffs (Defect,    Defect)    = \case {Player1 -> 1; Player2 -> 1}

pd :: ((PD, PD) <| Bool) ~> ()
pd = reverse (uncurry (&&))
   . unpack
   . ((argmax Player1 [Cooperate,Defect]) `alongside` (argmax Player2 [Cooperate,Defect]))
   . nashator
   . banglift pdPayoffs
   . bangcounit

pd' :: ((PD, PD) <| Bool) ~> ()
pd' = reverse (uncurry (&&))
    . unpack
    . strength (argmax Player1 [Cooperate,Defect])
    . bilift (\(x, b) -> (b, x)) (\(b', x') -> (x', b'))
    . strength (argmax Player2 [Cooperate,Defect])
    . bilift (\(y, x) -> (x, y)) (\(y', x') -> (x', y'))
    . nashator
    . banglift pdPayoffs
    . bangcounit
