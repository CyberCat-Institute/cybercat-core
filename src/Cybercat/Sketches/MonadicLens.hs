{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}

module Cybercat.Sketches.MonadicLens where

import Control.Monad
import Data.Functor.Const
import Control.Lens

class RightModule m f where
    act :: f (m a) -> f a

instance (Monad m) => RightModule m m where
    act = join

-- We need IncoherentInstances for this
-- I *think* because ghc can't figure out that Monad (Const a) can't be true
-- So I *think* this is the mythical safe usage of IncoherentInstances
instance RightModule m (Const a) where
    act = Const . getConst

type MonadicLens m s t a b = forall f. (Functor f, RightModule m f) => LensLike f s t a b

monadicGet :: MonadicLens m s t a b -> s -> a
monadicGet l s = getConst (l (Const) s)

monadicPut :: (Monad m) => MonadicLens m s t a b -> s -> b -> m t
monadicPut l s b = l (const (return b)) s

monadicLens :: (Monad m) => (s -> a) -> (s -> b -> m t) -> MonadicLens m s t a b
monadicLens g p k s = act (fmap (p s) (k (g s)))
