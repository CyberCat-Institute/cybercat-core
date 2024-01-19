{-# LANGUAGE RankNTypes #-}

module CyberCat.Sketches.Iteration where

import Control.Lens

data Iterator s t = Iterator {
    initialState :: s,
    updateState :: t -> s
}

iteratorApply :: Lens s t a b -> Iterator s t -> Iterator a b
iteratorApply l (Iterator s f) = Iterator (s ^# l) (\b -> (f (s & l .~ b)) ^# l)

iteratorRun :: Iterator s t -> Lens s t () () -> [s]
iteratorRun (Iterator s f) l = s : iteratorRun (Iterator (f (s & l .~ ())) f) l

{-
Dinaturality law:

iteratorRun i (l . k)
===
map (^# l) (iteratorRun (iteratorApply l i) k)
-}

{-
Example:

> take 10 $ iteratorRun (Iterator 0 id) (lens (const ()) (const . (+ 1)))
=> [0,1,2,3,4,5,6,7,8,9]
-}
