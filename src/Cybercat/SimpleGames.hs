{-# LANGUAGE TypeOperators #-}

module Cybercat.SimpleGames where

import Cybercat.Core

type Payoffs p = p -> Double

argmax1 :: p -> [x] -> x |> Bool ~> I (x |> Payoffs p)
argmax1 p xs (Forward x) = I (Forward x)
                        :> \u -> all (\x' -> u (Forward x) p >= u (Forward x') p) xs

argmax :: p -> [y] -> (x -> y, x) |> Bool ~> I (y |> Payoffs p)
argmax p ys (Forward (f, x)) = let y = Forward (f x)
                                in I y :> \u -> all (\y' -> u y p >= u (Forward y') p) ys
