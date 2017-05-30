import Control.Comonad

data Test a = Test [a] a [a]

instance Comonad Test where
  extract (Test a b c) = b
