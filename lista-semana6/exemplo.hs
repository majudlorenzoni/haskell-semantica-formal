data N = Zero | Suc N
deriving(Eq,Show)

soma :: N -> N -> N
soma Zero n = n
soma (Suc n1) n2 = soma n1 (Suc n2)

