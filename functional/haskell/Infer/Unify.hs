module Unify where

type Id = String
data Term = TypeVar Id | Arrow Id [Term] deriving (Show,Eq)
--Sub: no id on lhs in an earlier term in the list
--so NOT: Sub [('a',Var 'b'),('b',Var 'c')] ?
-- instead: Sub [('b',Var 'c'),('a',Var 'b')]
--BECAUSE: subs are applied using foldr going last -> first
data Substitution = Sub [(Id, Term)] deriving (Show,Eq)

--verifySub :: Substitution -> Bool .... ?
-- check that no id is in a typevar or arrow (recursively)
-- from back to front
verifySub (Sub l)
  | l == [] = True
  | otherwise = verifyRecursive id xs where
      ((id,term):xs) = reverse l
      
verifyRecursive id list
  | list == [] = True
  | otherwise = curr && remainder where
      curr = not $ True `elem` map (\(_,t) -> occurs id t) list
      remainder = verifyRecursive (fst $ head list) (tail list)
  

--occurs :: Id -> Term -> Bool
occurs x t = case t of
  TypeVar y -> x == y
  Arrow _ s -> True `elem` map (occurs x) s

--substitute :: Term -> Id -> Term -> Term
--substitute s for cases of x in t
substitute s x t = case t of
  TypeVar y -> if x == y then s else t
  Arrow f u -> Arrow f $ map (substitute s x) u

--apply :: Substitution -> Term -> Term
apply (Sub s) t = foldr (\(x, u) t' -> substitute u x t') t s

--unifyOne :: Term -> Term -> Sub
unifyOne s t = case (s,t) of
  (TypeVar x, TypeVar y) -> if x == y
                    then Sub []
                    else Sub [(x,t)]

  (Arrow f sc, Arrow g tc) -> if f == g && length sc == length tc
                          then unify $ zip sc tc
                          else error "Head Conflict"

  (TypeVar x, Arrow f sc) -> if occurs x t
                       then error "Circular var to app"
                       else Sub [(x,t)]

  (Arrow f sc, TypeVar x) -> if occurs x s
                       then error "Circular app to var"
                       else Sub [(x,s)]

--unify :: [(Term, Term)] -> Substitution
unify s = case s of
  [] -> Sub []
  (x,y) : rst -> Sub $ t1 ++ t2 where
    Sub t2 = unify rst
    Sub t1 = unifyOne (apply (Sub t2) x) (apply (Sub t2) y)
