{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RebindableSyntax #-}

module Cybercatv3 where

import Prelude hiding (return, (>>=), reverse)

import Cybercat (PDMove(..), Player(..))
import Cybercatv2 (type (|>)(..), type (<|), forward, Reverse, Payoffs)

data ConcreteMonocle x y = ConcreteMonocle {
  view :: x -> y,
  update :: x -> (y -> Reverse y) -> (x -> Reverse x)
}

return :: x -> ConcreteMonocle (() <| ()) (x <| s)
return x = ConcreteMonocle {
  view = const (Forward x),
  update = const (const (const ()))
}

(>>=) :: (Reverse x ~ Reverse y) => ConcreteMonocle a x -> (x -> ConcreteMonocle b y) -> ConcreteMonocle (a, b) y
(>>=) (ConcreteMonocle v u) f = ConcreteMonocle {
  view = \(a, b) -> view (f (v a)) b,
  update = \(a, b) k (a', b') -> (u a (\x -> k (view (f x) b)) a',
                                  update (f (v a)) b k b')
}

para :: ConcreteMonocle x x
para = ConcreteMonocle id (flip const)

costate :: ConcreteMonocle (x <| x) (() <| ())
costate = ConcreteMonocle {
  view = const (Forward ()),
  update = const (const runForward)
}

reverse :: (r -> s) -> ConcreteMonocle (x <| s) (x <| r)
reverse f = ConcreteMonocle {
  view = forward,
  update = \x k x' -> f (k (forward x))
}

argmax :: Player -> [x] -> ConcreteMonocle (x <| Bool) (x <| Payoffs)
argmax name xs = ConcreteMonocle {
  view = forward,
  update = const (\p x -> all (\x' -> p (forward x) name >= p (Forward x') name) xs)
}

--foo :: ConcreteMonocle (PDMove <| Bool) (() <| ())
foo = do a <- para
         return ()
