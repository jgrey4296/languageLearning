module Unify where
type Id = String
data Term = Var Id | App Id [Term] deriving (Show,Eq)
data Sub = Sub [(Id, Term)] deriving (Show,Eq)
--occurs :: Id -> App -> Bool
occurs x t = case t of
  Var y -> x == y
  App _ s -> True `elem` map (occurs x) s
--substitute :: Term -> Id -> Term -> Term
substitute s x t = case t of
  Var y -> if x == y then s else t
  App f u -> App f $ map (substitute s x) u
--apply :: Sub -> Term -> Term
apply (Sub s) t = foldr (\(x, u) t' -> substitute u x t') t s
--unifyOne :: Term -> Term -> Sub
unifyOne s t = case (s,t) of
  (Var x, Var y) -> if x == y
                    then Sub []
                    else Sub [(x,t)]

  (App f sc, App g tc) -> if f == g && length sc == length tc
                          then unify $ zip sc tc
                          else error "Head Conflict"

  (Var x, App f sc) -> if occurs x t
                       then error "Circular"
                       else Sub [(x,t)]

  (App f sc, Var x) -> if occurs x t
                       then error "Circular"
                       else Sub [(x,t)]
--unify :: [(Term, Term)] -> Sub
unify s = case s of
  [] -> Sub []
  (x,y) : rst -> Sub $ t1 ++ t2 where
    Sub t2 = unify rst
    Sub t1 = unifyOne (apply (Sub t2) x) (apply (Sub t2) y)
