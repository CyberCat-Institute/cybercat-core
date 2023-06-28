{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}

module Cybercat.Sketches.MonadicOptic where

import Control.Monad
import Control.Lens

import Cybercat.Sketches.MonadicLens

data ExistentialKleisliOptic m s t a b where
    ExistentialKleisliOptic :: (s -> m (a, z)) 
                            -> (z -> b -> m t) 
                            -> ExistentialKleisliOptic m s t a b

(>>>) :: (Monad m)
      => ExistentialKleisliOptic m s t a b
      -> ExistentialKleisliOptic m a b p q
      -> ExistentialKleisliOptic m s t p q
(ExistentialKleisliOptic g1 p1) >>> (ExistentialKleisliOptic g2 p2)
  = ExistentialKleisliOptic (\s -> do (a, z1) <- g1 s
                                      (p, z2) <- g2 a
                                      pure (p, (z1, z2)))
                            (\(z1, z2) q -> do b <- p2 z2 q
                                               p1 z1 b)

-- Distributive law of a monad over a functor
-- Must satisfy 2 axioms:
-- (1) distribute . pure = fmap pure
-- (2) distribute . join = fmap join . distribute . fmap distribute
-- See https://ncatlab.org/nlab/show/distributive+law#explicit_definition
class Distributive m f where
    distribute :: m (f a) -> f (m a)

instance (Monad m) => Distributive m (Const (m a)) where
    distribute a = Const $ do a' <- a
                              getConst a'

-- Distributive law of a monad over its underlying functor!
instance (Monad m) => Distributive m m where
--  distribute = pure . join
  distribute = fmap pure . join

type KleisliOptic m s t a b = forall f. (Functor f, RightModule m f, Distributive m f) 
                                     => LensLike f s t a b

kleisliGet :: (Monad m) => KleisliOptic m s t a b -> s -> m a
kleisliGet l s = getConst (l (Const . pure) s)

kleisliPut :: (Monad m) => KleisliOptic m s t a b -> s -> b -> m t
kleisliPut l s b = l (const (pure b)) s

kleisliOptic :: (Monad m) => ExistentialKleisliOptic m s t a b -> KleisliOptic m s t a b
kleisliOptic (ExistentialKleisliOptic g p) k s
  = act $ fmap (>>= (uncurry p)) $ distribute $ do (a, z) <- g s
                                                   pure (fmap (z, ) (k a))

data Monomial m a b x = Monomial {runMonomial :: m (a, b -> m x)}
  deriving (Functor)

instance (Monad m) => RightModule m (Monomial m a b) where
  act (Monomial af) = Monomial $ do (a, f) <- af
                                    pure (a, join . f)

instance (Monad m) => Distributive m (Monomial m a b) where
  distribute maf = Monomial $ do Monomial af <- maf
                                 (a, f) <- af
                                 pure (a, pure . f)

unKleisliOptic :: (Monad m) => KleisliOptic m s t a b -> ExistentialKleisliOptic m s t a b
unKleisliOptic l = ExistentialKleisliOptic 
  (runMonomial . l (\a -> Monomial (pure (a, pure))))
  ($)
