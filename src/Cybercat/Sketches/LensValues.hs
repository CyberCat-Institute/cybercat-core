{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Cybercat.Sketches.LensValues where

import Control.Lens
import Control.Lens.Internal.Getter (AlongsideLeft(..), AlongsideRight(..))
import Control.Lens.Tutorial

type Value f a b t = (a -> f b) -> f t

value :: a -> Value f a b b
value a k = k a

buildLens :: (s -> Value f a b t) -> LensLike f s t a b
buildLens f k s = f s k

(/\) :: Value (AlongsideLeft f b') a b t 
     -> Value (AlongsideRight f t) a' b' t' 
     -> Value f (a, a') (b, b') (t, t')
(l /\ m) k = getAlongsideRight (m (\a' -> AlongsideRight 
             (getAlongsideLeft (l (\a -> AlongsideLeft (k (a, a')))))))

-- Same as Control.Lens.alongside
parallel :: Lens s t a b -> Lens s' t' a' b' -> Lens (s, s') (t, t') (a, a') (b, b')
parallel l m = buildLens $ \(s, s') -> let a = value s . l
                                           b = value s' . m
                                        in a /\ b

atpx :: Traversal' Molecule Double
atpx = buildLens $ \molecule -> let --atom :: Value _ [Atom] [Atom] Molecule
                                    atom = value molecule . atoms
                                    --traversal :: Value _ Atom Atom Molecule
                                    traversal = atom . traverse
                                    --xy :: Value _ Point Point Molecule 
                                    xy = traversal . point
                                 in xy . x

average :: Lens' (Double, Double) Double
average = lens (\(x, y) -> f x y) 
               (\(x, y) a -> let a' = f x y
                              in (x - a' + a, y - a' + a))
  where f x y = (x + y)/2

averageX :: Lens' (Atom, Atom) Double
averageX = buildLens $ \(a1, a2) -> let pos1 = value a1 . point
                                        pos2 = value a2 . point
                                        x1 = pos1 . x
                                        x2 = pos2 . x
                                     in (x1 /\ x2) . average
