type Id = String
-- AST Nodes
data Expr = Var Id
          | Fun Id Expr
          | App Expr Expr deriving (Show,Eq,Ord)

-- Types Expressions
data Type = TVar Id
          | Arrow Type Type deriving (Show,Eq,Ord)

-- Annotated expressions
data AExpr = AVar Id Type
               | AFun Id AExpr Type
               | AApp AExpr AExpr Type deriving (Show,Eq,Ord)
