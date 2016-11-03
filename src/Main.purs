module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Prelude (Unit)

-- class (Monoid m) <= Group m where
--   inverse :: m -> m
--
-- class (Group v, Field f) <= VectorSpace v f where
--   scalarMulti :: f -> v -> v
--
-- type Objective = ∀ v f. VectorSpace v f => v -> Number

type ScalarField = Array Number -> Number
type VectorField = Array Number -> Array Number
type Objective   =
  { arity :: Int
  , valueAt :: ScalarField
  , gradientAt :: VectorField
  }

data Exp
  = V Int
  | Plus Exp Exp
  | Multiply Exp Exp
  | Negate Exp
  | Divide Exp Exp
  | Pow Exp Number
  | Application Objective (Array Exp)

-- fold :: forall a. (Exp -> a -> a) -> a -> Exp -> a

-- fold :: (Exp -> Array Number -> Array Number) -> Array Number -> Exp -> Array Number

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
