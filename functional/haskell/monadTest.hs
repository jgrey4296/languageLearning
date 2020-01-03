import Control.Monad

data JGMaybe a = JGJust a | JGNothing deriving (Show, Eq)

instance Functor JGMaybe where
  fmap func (JGJust a) = JGJust $ func a
  fmap func JGNothing = JGNothing
  (<$) val (JGJust a) = JGJust val
  (<$) val JGNothing = JGNothing

instance Applicative JGMaybe where
  pure a = JGJust a
  (<*>) (JGJust f) (JGJust b) = JGJust $ f b
  (<*>) _ _ = JGNothing
  (*>) (JGJust a) b = b
  (*>) _ _ = JGNothing
  (<*) a (JGJust b) = a
  (<*) _ _ = JGNothing

instance Monad JGMaybe where
  (>>=) (JGJust a) func = func a
  (>>=) JGNothing func = JGNothing
  (>>) (JGJust a) b = b
  (>>) JGNothing _ = JGNothing
  return a = JGJust a
  
  
data JGVal a = JGVal a deriving (Show, Eq)

instance Functor JGVal where
  fmap func (JGVal a) = JGVal $ func a

instance Applicative JGVal where
  pure a = JGVal $ a
  (<*>) (JGVal f) (JGVal a) = JGVal $ f a

instance Monad JGVal where
  (>>=) (JGVal a) func = func a

