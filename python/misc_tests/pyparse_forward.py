import pyparsing as pp


name = pp.Word("blah")

func = pp.Forward()

args = pp.Or([name, func])

func << name + pp.Literal('(') + pp.Optional(func) + pp.Literal(')')
