{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Cybercat where

import Prelude hiding (id, (.))
import qualified Prelude (id, (.))
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Control.Category
import Control.Arrow
import Control.Lens

import Data.Functor.Identity

data Monocle f x y = Monocle (Lens x (f x) y (f y))

instance Category (Monocle f) where
  id = Monocle Prelude.id
  (Monocle g) . (Monocle f) = Monocle (f Prelude.. g)

instance (Contravariant f) => Arrow (Monocle f) where
  arr f = Monocle (\k -> fmap (contramap f) . k . f)
  first (Monocle l) = Monocle (\k (x, z) -> fmap (contramap fst) (l (\y -> fmap (contramap (, z)) (k (y, z))) x))

data ConcreteLens s t a b = ConcreteLens (s -> a) (s -> b -> t)

data ConcreteMonocle f x y = ConcreteMonocle (x -> y) (x -> f y -> f x)

concreteMonocleToLens :: ConcreteMonocle f x y -> ConcreteLens x (f x) y (f y)
concreteMonocleToLens (ConcreteMonocle f f') = ConcreteLens f f'

instance Category (ConcreteMonocle f) where
  id = ConcreteMonocle (Prelude.id) (curry snd)
  (ConcreteMonocle g1 g2) . (ConcreteMonocle f1 f2) = ConcreteMonocle (g1 . f1) (\x z -> f2 x (g2 (f1 x) z))

instance (Contravariant f) => Arrow (ConcreteMonocle f) where
  arr f = ConcreteMonocle f (const (contramap f))
  first (ConcreteMonocle l _) = ConcreteMonocle (\(x, z) -> (l x, z))
                                                (const (\a -> contramap (\(x, z) -> (l x, z)) a))
  {-}
  first (ConcreteMonocle f f') = ConcreteMonocle (\(x, z) -> (f x, z))
                                                 (\(x, z) -> contramap fst . f' x . contramap (, z))
-}

data Player = Player1 | Player2 deriving (Show, Eq)

data Payoffs x = Payoffs (x -> Player -> Double)

instance Contravariant Payoffs where
  contramap f (Payoffs p) = Payoffs (p . f)

costate :: f x -> Monocle f x ()
costate f = Monocle (iso (const ()) (const f))
--costate f = Monocle (lens (const ()) (const (const f)))

costateConcrete :: f x -> ConcreteMonocle f x ()
costateConcrete f = ConcreteMonocle (const ()) (const (const f))

costateSpecial :: ConcreteMonocle Payoffs (Player -> Double) ()
costateSpecial = ConcreteMonocle (const ()) (const (const (Payoffs id)))

argmax :: Player -> [x] -> Lens x (Predicate x) x (Payoffs x)
argmax name xs = iso id (\(Payoffs p) -> Predicate (\x -> all (\x' -> p x name >= p x' name) xs))
--argmax name xs = lens id (const (\(Payoffs p) -> Predicate (\x -> all (\x' -> p x name >= p x' name) xs)))

argmaxConcrete :: Player -> [x] -> ConcreteLens x (Predicate x) x (Payoffs x)
argmaxConcrete name xs = ConcreteLens id
                                      (const (\(Payoffs p) -> Predicate (\x -> all (\x' -> p x name >= p x' name) xs)))

runStrategy :: (Contravariant f) => Monocle f (x -> y, x) y
runStrategy = arr (uncurry ($))

runStrategyConcrete :: (Contravariant f) => ConcreteMonocle f (x -> y, x) y
runStrategyConcrete = arr (uncurry ($))

data PDMove = Cooperate | Defect deriving (Show, Eq)
data CoordinationMove = ESB | GCS deriving (Show, Eq)
data UltimatumP1Move = Fair | Unfair deriving (Show, Eq)
data UltimatumP2Move = Accept | Reject deriving (Show, Eq)


{-}
pdPayoffs :: Monocle Payoffs (PDMove, PDMove) ()
pdPayoffs = Monocle (\k _ -> fmap (\_ -> Payoffs foo) (k ()))
  where foo (Cooperate, Cooperate) = \case {Player1 -> 2; Player2 -> 2}
        foo (Cooperate, Defect)    = \case {Player1 -> 0; Player2 -> 3}
        foo (Defect,    Cooperate) = \case {Player1 -> 3; Player2 -> 0}
        foo (Defect,    Defect)    = \case {Player1 -> 1; Player2 -> 1}
-}

pdPayoffs :: Payoffs (PDMove, PDMove)
pdPayoffs = Payoffs p where
  p (Cooperate, Cooperate) = \case {Player1 -> 2; Player2 -> 2}
  p (Cooperate, Defect)    = \case {Player1 -> 0; Player2 -> 3}
  p (Defect,    Cooperate) = \case {Player1 -> 3; Player2 -> 0}
  p (Defect,    Defect)    = \case {Player1 -> 1; Player2 -> 1}

ultimatumPayoffs :: Payoffs (UltimatumP1Move, UltimatumP2Move)
ultimatumPayoffs = Payoffs p where
  p (Fair, Accept) _ = 5
  p (Unfair, Accept) Player1 = 8
  p (Unfair, Accept) Player2 = 2
  p (_, Reject) _ = 0

{-}
ultimatumPayoffs :: Monocle Payoffs (UltimatumP1Move, UltimatumP2Move) ()
ultimatumPayoffs = Monocle (lens (const ()) (const (const (Payoffs p))))
  where p (Fair, Accept) _ = 5
        p (Unfair, Accept) Player1 = 8
        p (Unfair, Accept) Player2 = 2
        p (_, Reject) _ = 0
        -}

coordinationPayoffs :: Monocle Payoffs (CoordinationMove, CoordinationMove) ()
coordinationPayoffs = Monocle (\k _ -> fmap (\_ -> Payoffs foo) (k ()))
  where foo (ESB, ESB) _ = 1
        foo (GCS, GCS) _ = 1
        foo _ _ = 0

pdArena :: ConcreteMonocle Payoffs (PDMove, PDMove) ()
pdArena = proc (a, b) -> do
  costateConcrete pdPayoffs -< (a, b)

ultimatumArena :: ConcreteMonocle Payoffs (() -> UltimatumP1Move, UltimatumP1Move -> UltimatumP2Move) ()
ultimatumArena = proc (a, b) -> do
  payoffs <- arr (\(a, b) -> let {x = a (); y = b x} in u (x, y)) -< (a, b)
  costateSpecial -< payoffs
  where Payoffs u = ultimatumPayoffs

coordinationArena :: Monocle Payoffs (CoordinationMove, CoordinationMove) ()
coordinationArena = proc (a, b) -> do
  coordinationPayoffs -< (a, b)
  returnA -< ()

sandwich :: (Divisible f, Contravariant g) => Lens x (f x) y (g y) -> Lens x' (f x') y' (g y') -> Lens (x, x') (f (x, x')) (y, y') (g (y, y'))
sandwich l m = (iso id (uncurry divided)) . (l `alongside` m) . (lens id nash)
  where nash (y, y') a = (contramap (, y') a, contramap (y,) a)

sandwichConcrete :: (Divisible f, Contravariant g)
                 => ConcreteLens x (f x) y (g y) -> ConcreteLens x' (f x') y' (g y')
                 -> ConcreteLens (x, x') (f (x, x')) (y, y') (g (y, y'))
sandwichConcrete (ConcreteLens l l') (ConcreteLens m m') = ConcreteLens lm lm' where
  lm (x, y) = (l x, m y)
  lm' (x, y) xy' = divided (l' x (contramap (, m y) xy')) (m' y (contramap (l x,) xy'))

result :: Monocle Payoffs (x, x') () -> [x] -> [x'] -> (x, x') -> (x, x') -> Bool
result arena as bs x = p
  where Monocle l = arena
        m = (argmax Player1 as `sandwich` argmax Player2 bs) . l
        k () = Identity (Payoffs (const (const 0)))
        Identity (Predicate p) = m k x

results :: Monocle Payoffs (x, x') () -> [x] -> [x'] -> [((x, x'), (x, x'))]
results arena as bs = filter (uncurry (result arena as bs))
                             [((a, b), (a', b')) | a <- as, b <- bs, a' <- as, b' <- bs]

as :: [() -> UltimatumP1Move]
as = [\() -> Fair, \() -> Unfair]

bs :: [UltimatumP1Move -> UltimatumP2Move]
bs = [const Accept, \case {Fair -> Accept; Unfair -> Reject},
      const Reject, \case {Fair -> Reject; Unfair -> Accept}]

coordinationMoves :: [CoordinationMove]
coordinationMoves = [ESB, GCS]
