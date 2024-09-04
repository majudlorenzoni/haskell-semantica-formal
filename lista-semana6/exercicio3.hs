data Nat = Zero | Suc Nat
  deriving(Eq, Show)



mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult a (Suc b) = soma a (mult a b)

soma :: Nat -> Nat -> Nat
soma Zero n = n
soma (Suc m) n = Suc (soma m n)

