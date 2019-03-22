from pyparsing import Word, alphas

greet = Word(alphas) + "," + Word(alphas) + "!"
hello = "Hello, world!"

print(hello, "->", greet.parseString(hello))
