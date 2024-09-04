data Nat = Zero | Suc Nat
  deriving(Eq, Show)

to_int :: Nat -> Int
to_int Zero = 0
to_int (Suc n) = 1 + to_int n