{-# LANGUAGE RankNTypes #-}

module Types(Expand) where
    import Data.Array.Accelerate
    type Expand a b = (Elt a, Elt b) => (Exp a -> Exp Int) -> (Exp a -> Exp Int -> Exp b) -> Acc (Vector a) -> Acc (Vector b)