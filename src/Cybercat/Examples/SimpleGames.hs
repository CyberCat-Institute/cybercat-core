{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Cybercat.Examples.SimpleGames where

import Cybercat.Core
import Cybercat.SimpleGames

data Player = Player1 | Player2 deriving (Show)

data PD = C | D deriving (Show)

pdPayoffs :: (PD, PD) -> Payoffs Player
pdPayoffs (C, C) = \case {Player1 -> 2; Player2 -> 2}
pdPayoffs (C, D) = \case {Player1 -> 0; Player2 -> 3}
pdPayoffs (D, C) = \case {Player1 -> 3; Player2 -> 0}
pdPayoffs (D, D) = \case {Player1 -> 1; Player2 -> 1}

pd :: (PD, PD) |> Bool ~> ()
pd strategies = let (a, b) = unpair' (backward (uncurry (&&)) strategies)
                    x = argmax1 Player1 [C, D] <~ a
                    y = argmax1 Player2 [C, D] <~ b
                    u = liftI' pdPayoffs <~ (x # y)
                 in target <~ u

-- To test a strategy profile:
-- > continueValue (pd (Forward (D,D))) ()
-- ==> True
