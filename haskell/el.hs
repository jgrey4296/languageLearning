data ELType = BANG | DOT deriving(Show, Eq)
type Symbol = String

data ELStringElement = Node{
  eltype :: ElType,
  sym :: Symbol
  }

type ELString = [ELStringElement]

data 

