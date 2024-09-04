data Nat = Zero | Suc Nat
  deriving(Eq, Show)

to_nat :: Int -> Nat
to_nat 0 = Zero
to_nat n = Suc (to_nat (n - 1))