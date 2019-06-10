"""
Run Tests on the Parser and Inference alg
"""
import IPython
import unittest
import logging
import parser as p
import trie as T
from terms import ExOp, Rule
from ex_types import TypeDefinition
from environment import Environment
import type_exceptions as te

class ParserTesting(unittest.TestCase):

    def test_parse_word(self):
        result = p.safeParse(p.CONST, ".blah")[0]
        self.assertEqual(result.name, "blah")
        self.assertEqual(result.ex, ExOp.DOT)
        self.assertIsNone(result._type)

    def test_parse_word_typed(self):
        result = p.safeParse(p.CONST, ".blah(::Test)")[0]
        self.assertEqual(result.name, "blah")
        self.assertEqual(result.ex, ExOp.DOT)
        self.assertEqual(str(result._type.name), ".Test")

    def test_parse_type_definition(self):
        the_string = "::MyType:\n .a.$x(::Blah)\n .b!$y(::Bloo)\nEND"
        result = p.safeParse(p.TYPEDEF, the_string)[0]
        self.assertEqual(str(result.name), ".MyType")
        self.assertEqual(len(result.structure), 2)

    def test_parse_simple_rule(self):
        the_string = " .a.rule(::Rule):\n .a.b.$x(::Something)\n .q.r.$x\n .q.w!$y\n END"
        result = p.safeParse(p.RULE, the_string)[0]
        self.assertEqual(result.name, ".a.rule")

    def test_parse_multiple_type_definitions(self):
        the_string = """

    ::test.person:
        .name.$x(::String)
        .age.$y(::Number)
    END

    ::test.location:
        .name.$x(::String)
        .size.$y(::String)
    END
        """
        result = p.safeParse(p.MAIN, the_string)
        self.assertEqual(len(result), 2)

    def test_parse_multiple_mixed_elements(self):
        the_string = """
        ::test.person:
        	.name.$x(::String)
            .age.$y(::Number)
        END

        .a.test.sentence.$x(::test.person)

        ::test.location:
            .name.$x(::String)
            .size.$y(::String)
        END

        .a.test.rule(::rule):
            .a.test.sentence.$q
            .$q.name.$y
        END
        """
        result = p.safeParse(p.MAIN, the_string)
        typedefs = [x for x in result if isinstance(x, TypeDefinition)]
        self.assertEqual(len(typedefs), 2)
        rules = [x for x in result if isinstance(x, Rule)]
        self.assertEqual(len(rules), 1)
        declarations = [x for x in result if isinstance(x, list)]
        self.assertEqual(len(declarations), 1)

    def test_construct_base_environment(self):
        the_string = """
        ::String: END
        ::Number: END
        ::test.person:
        	.name.$x(::String)
            .age.$y(::Number)
        END

        .a.test.sentence.$x(::test.person)

        ::test.location:
            .name.$x(::String)
            .size.$y(::String)
        END
        """
        result = p.safeParse(p.MAIN, the_string)
        env = Environment(result[:])
        #check type equations are added
        self.assertEqual(len(env.type_equations), 4)
        #Check type assignments are added
        self.assertEqual(len(env.type_assignments), 1)

    def test_update_environment(self):
        the_string = """
        ::String: END
        ::Number: END
        ::test.person:
        	.name.$x(::String)
            .age.$y(::Number)
        END

        .a.test.sentence.$x(::test.person)

        ::test.location:
            .name.$x(::String)
            .size.$y(::String)
        END
        """
        result = p.safeParse(p.MAIN, the_string)
        env = Environment(result[:])
        #check type equations are added
        self.assertEqual(len(env.type_equations), 4)
        #Check type assignments are added
        self.assertEqual(len(env.type_assignments), 1)
        result2 = p.safeParse(p.MAIN, ".a.second.test.$y(::test.location)")[0]
        env.add(result2)
        self.assertEqual(len(env.type_assignments), 2)

        result3 = p.safeParse(p.MAIN, "::test.new:\n .name.$x(::String)\nEND")[0]
        env.add(result3)
        self.assertEqual(len(env.type_equations), 5)

    def test_update_environment_conflict(self):
        the_string = """
        ::test.person:
        	.name.$x(::String)
            .age.$y(::Number)
        END

        ::test.person:
            .name.$x(::String)
            .size.$y(::String)
        END
        """
        result = p.safeParse(p.MAIN, the_string)
        with self.assertRaises(te.TypeRedefinitionException):
            env = Environment(result[:])

    def test_update_environment_conflict_2(self):
        the_string = """
        ::first: END
        ::second: END

        .a.test(::first)
        .a.test(::second)
        """
        result = p.safeParse(p.MAIN, the_string)
        with self.assertRaises(te.TypeConflictException):
            env = Environment(result[:])

    def test_update_environment_conflict_3(self):
        the_string = """
        .a.test(::first)
        """
        result = p.safeParse(p.MAIN, the_string)
        with self.assertRaises(te.TypeUndefinedException):
            env = Environment(result[:])
            env.validate()

    def test_infer_type(self):
        the_string = """
        ::first: END
        .a.$y(::first)
        .a.$x
        """
        result = p.safeParse(p.MAIN, the_string)
        env = Environment(result[:])
        env.validate()
        queried = env.type_assignments.query(p.safeParse(p.MAIN, ".a.$x")[0])
        IPython.embed(simple_prompt=True)
        self.assertEqual(queried._type, result[1][-1]._type)

    def test_infer_component(self):
        the_string = """
        ::String: END
        ::first:
            .name.$x(::String)
        END

        .a.$x(::first)
        .$x.name.$y
        """
        result = p.safeParse(p.MAIN, the_string)
        env = Environment(result[:])
        env.validate()

if __name__ == "__main__":
    #use python $filename to use this logging setup
    LOGLEVEL = logging.INFO
    logFileName = "log.parser_testing"
    logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
    console = logging.StreamHandler()
    console.setLevel(logging.INFO)
    logging.getLogger().addHandler(console)
    unittest.main()
    #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
