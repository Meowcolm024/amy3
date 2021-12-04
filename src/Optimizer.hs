module Optimizer
    ( optimize
    ) where

import           Types

optimize :: Bool -> Expr a -> Expr a
optimize True = constFold
optimize False = id

-- | constant folding optimization
constFold :: Expr a -> Expr a
constFold = id
