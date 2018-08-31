{- 

HaskellBase.hs: Pickling/unpickling primitives for Haskell

Fermin Reig Galilea
University of Glasgow
http://www.dcs.gla.ac.uk/~reig/

13-1-1998
Modified by Daniel Wang for new asdlGen naming convention.
-}

module StdPrims(Int, String, Identifier, Big_int)  where

import Prelude
type Identifier = String
type Big_int    = Integer
