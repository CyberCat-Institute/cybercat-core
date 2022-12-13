{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Cybercat.Lens where

import Control.Lens (Lens, (^#), (.~))

import Cybercat.Core

toLens :: x ~> y -> Lens x (Reverse x) y (Reverse y)
toLens l k x = let y :> u = l x
                in fmap u (k y)

fromLens :: Lens x (Reverse x) y (Reverse y) -> x ~> y
fromLens l x = (x ^# l) :> (\y' -> (l .~ y') x)
