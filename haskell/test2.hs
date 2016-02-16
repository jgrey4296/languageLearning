class Tofu t where
  tofu :: j a -> t j a

data Frank a b = Frank { frankField :: a b } deriving (Show)

instance Tofu Frank where
  tofu x = Frank x
