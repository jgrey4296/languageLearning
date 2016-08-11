import Unify

a = Var "a"
b = Var "b"
c = App "c" [a,b]

aSub = Sub [("a",b),("b",c)]

-- Test Occurs
t1 = occurs "a" c == True
t2 = occurs "d" c == False
t1b = occurs "a" a == True
t2b = occurs "c" c == False -- the id of an app doesnt count

-- test substitution
t3 = substitute b "a" a == b
t4 = substitute c "a" a == c
t5 = substitute b "a" c == App "c" [b,b]

--test application
t6 = apply aSub a == b
t7 = apply aSub b == App "c" [b,b]
t8 = apply aSub c == App "c" [b, App "c" [ b, b] ]


tests = [
  t1,t2,t1b,t2b,t3,t4,t5,t6,t7,t8
  ]

test = all id tests
