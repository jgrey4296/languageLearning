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
        result = p.safeParse(p.WORD, ".blah")[0]
        self.assertEqual(result.name, "blah")
        self.assertEqual(result.ex, ExOp.DOT)
        self.assertIsNone(result._type)

    def test_parse_word_typed(self):
        result = p.safeParse(p.WORD, ".blah(::.Test)")[0]
        self.assertEqual(result.name, "blah")
        self.assertEqual(result.ex, ExOp.DOT)
        self.assertEqual(repr(result._type.name), ".Test")

    def test_parse_word_nested_types(self):
        result = p.safeParse(p.WORD, ".blah(::.First(::.Second))")[0]
        self.assertEqual(result.name, "blah")
        self.assertEqual(result.ex, ExOp.DOT)
        self.assertEqual(repr(result._type.name), ".First")
        self.assertEqual(len(result._type._args), 1)
        self.assertEqual(repr(result._type._args[0].name), ".Second")

    def test_parse_word_nested_types_multiple(self):
        result = p.safeParse(p.WORD, ".blah(::.First(::.Second, ::.Third))")[0]
        self.assertEqual(result.name, "blah")
        self.assertEqual(result.ex, ExOp.DOT)
        self.assertEqual(repr(result._type.name), ".First")
        self.assertEqual(len(result._type._args), 2)
        self.assertEqual(repr(result._type._args[0].name), ".Second")
        self.assertEqual(repr(result._type._args[1].name), ".Third")

    def test_parse_type_definition(self):
        the_string = "::.MyType:\n .a.$x(::.Blah)\n .b!$y(::.Bloo)\nEND"
        result = p.safeParse(p.TYPEDEF, the_string)[0]
        self.assertEqual(repr(result.name), ".MyType")
        self.assertEqual(len(result.structure), 2)

    def test_parse_simple_rule(self):
        the_string = " .a.rule:\n .a.b.$x(::.Something)\n .q.r.$x\n .q.w!$y\n END"
        result = p.safeParse(p.RULE, the_string)[0]
        self.assertEqual(repr(result), "Rule: '.a.rule'")

    def test_parse_multiple_type_definitions(self):
        the_string = """

    ::.test.person:
        .name.$x(::.String)
        .age.$y(::.Number)
    END

    ::.test.location:
        .name.$x(::.String)
        .size.$y(::.String)
    END
        """
        result = p.safeParse(p.MAIN, the_string)
        self.assertEqual(len(result), 2)

    def test_parse_multiple_mixed_elements(self):
        the_string = """
        ::.test.person:
        	.name.$x(::.String)
            .age.$y(::.Number)
        END

        .a.test.sentence.$x(::.test.person)

        ::.test.location:
            .name.$x(::.String)
            .size.$y(::.String)
        END

        .a.test.rule(::.rule):
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

    def test_parse_typedef_with_vars(self):
        the_string = """
        ::.polytype[$x]:
            .name.$x
            .string.$x
        END

        """
        result = p.safeParse(p.MAIN, the_string)[0]
        self.assertEqual(repr(result.name), ".polytype")
        self.assertEqual(len(result.structure), 2)
        self.assertEqual(len(result._vars), 1)


    def test_construct_base_environment(self):
        the_string = """
        ::.String: END
        ::.Number: END
        ::.test.person:
        	.name.$x(::.String)
            .age.$y(::.Number)
        END

        .a.test.sentence.$x(::.test.person)

        ::.test.location:
            .name.$x(::.String)
            .size.$y(::.String)
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
        ::.String: END
        ::.Number: END
        ::.test.person:
        	.name.$x(::.String)
            .age.$y(::.Number)
        END

        .a.test.sentence.$x(::.test.person)

        ::.test.location:
            .name.$x(::.String)
            .size.$y(::.String)
        END
        """
        result = p.safeParse(p.MAIN, the_string)
        env = Environment(result[:])
        #check type equations are added
        self.assertEqual(len(env.type_equations), 4)
        #Check type assignments are added
        self.assertEqual(len(env.type_assignments), 1)
        result2 = p.safeParse(p.MAIN, ".a.second.test.$y(::.test.location)")[0]
        env.add(result2)
        self.assertEqual(len(env.type_assignments), 2)

        result3 = p.safeParse(p.MAIN, "::.test.new:\n .name.$x(::.String)\nEND")[0]
        env.add(result3)
        self.assertEqual(len(env.type_equations), 5)

    def test_update_environment_conflict(self):
        the_string = """
        ::.test.person:
        	.name.$x(::.String)
            .age.$y(::.Number)
        END

        ::.test.person:
            .name.$x(::.String)
            .size.$y(::.String)
        END
        """
        result = p.safeParse(p.MAIN, the_string)
        with self.assertRaises(te.TypeRedefinitionException):
            env = Environment(result[:])

    def test_update_environment_conflict_2(self):
        the_string = """
        ::.first: END
        ::.second: END

        .a.test(::.first)
        .a.test(::.second)
        """
        result = p.safeParse(p.MAIN, the_string)
        with self.assertRaises(te.TypeConflictException):
            env = Environment(result[:])

    def test_update_environment_conflict_3(self):
        the_string = """
        .a.test(::.first)
        """
        result = p.safeParse(p.MAIN, the_string)
        with self.assertRaises(te.TypeUndefinedException):
            env = Environment(result[:])
            env.validate()


    def test_infer_type(self):
        the_string = """
        ::.first: END
        .a.$y(::.first)
        .a.$x
        """
        result = p.safeParse(p.MAIN, the_string)
        env = Environment(result[:])
        env.validate()
        queried = env.type_assignments.query(p.safeParse(p.MAIN, ".a.$x")[0])
        self.assertEqual(queried._type, result[1][-1]._type)

    def test_infer_component(self):
        the_string = """
        ::.String: END
        ::.first:
            .name.$x(::.String)
        END

        .a.$x(::.first)
        .$x.name.$y
        """
        result = p.safeParse(p.MAIN, the_string)
        env = Environment(result[:])
        env.validate()
        env.query(p.safeParse(p.MAIN, ".$x (::.first)"))
        env.query(p.safeParse(p.MAIN, ".$x.name.$y(::.String)"))

    def test_type_conflict(self):
        the_string = """
        ::.String: END
        ::.Number: END
        .a.$x(::.String)
        .a.$y(::.Number)
        """
        result = p.safeParse(p.MAIN, the_string)
        env = Environment(result[:])
        with self.assertRaises(te.TypeConflictException):
            env.validate()

    def test_type_undefined(self):
        the_string = """
        ::.String: END
        ::.Number: END
        .a.$x(::.String)
        .b.$y(::.Blah)
        """
        result = p.safeParse(p.MAIN, the_string)
        env = Environment(result[:])
        with self.assertRaises(te.TypeUndefinedException):
            env.validate()

    def test_type_redefinition(self):
        the_string = """
        ::.String: END
        ::.String: END
        .a.$x(::.String)
        """
        result = p.safeParse(p.MAIN, the_string)
        with self.assertRaises(te.TypeRedefinitionException):
            env = Environment(result[:])

    def test_variable_conflict(self):
        the_string = """
        ::.String: END
        ::.Number: END
        .a.$x(::.String)
        .b.$x(::.Number)
        """
        result = p.safeParse(p.MAIN, the_string)
        with self.assertRaises(te.TypeConflictException):
            env = Environment(result[:])

    def test_structure_mismatch(self):
        the_string = """
        ::.String: END
        ::.Number: END
        ::.first:
            .name.$x(::.String)
        END

        .a(::.first).b
        """
        result = p.safeParse(p.MAIN, the_string)
        env = Environment(result[:])
        with self.assertRaises(te.TypeStructureMismatch):
            env.validate()

    def test_structure_type_conflict(self):
        the_string = """
        ::.String: END
        ::.Number: END
        ::.first:
            .name.$x(::.String)
        END

        .a(::.first).name.$y(::.Number)
        """
        result = p.safeParse(p.MAIN, the_string)
        env = Environment(result[:])
        with self.assertRaises(te.TypeConflictException):
            env.validate()

    def test_typing_1(self):
        the_string = """
        ::.String: END
        ::.Number: END

        ::.a.test.type:
            .name.$x(::.String)
            .age.$y(::.Number)
        END

        ::.second.type:
            .name.$x(::.String)
            .age.$y(::.String)
        END

        .bob(::.a.test.type).age.$z
        .bill(::.second.type).age.$q
        """
        result = p.safeParse(p.MAIN, the_string)
        env = Environment(result[:])
        env.validate()
        self.assertTrue(env.query(p.safeParse(p.MAIN, ".bob.age.$z(::.Number)\n .bill.age.$q(::.String)")))

    def test_typing_nested_vars(self):
        the_string = """
        ::.String: END
        ::.Number: END

        ::.a.test.type:
            .name.$x(::.String).$y(::.Number)
        END

        .bob(::.a.test.type).name.$z.$q
        """
        result = p.safeParse(p.MAIN, the_string)
        env = Environment(result[:])
        env.validate()
        self.assertTrue(env.query(p.safeParse(p.MAIN, ".bob.name.$z(::.String)\n .bob.name.$z.$q(::.Number)")))

    def test_typing_nested_types(self):
        the_string = """
        ::.String: END
        ::.Number: END

        ::.small.type:
            .name.$x(::.String)
        END

        ::.large.type:
           .component.$x(::.small.type)
        END

        .a(::.large.type).component.$q.name.$w
        """
        result = p.safeParse(p.MAIN, the_string)
        env = Environment(result[:])
        env.validate()
        self.assertTrue(env.query(p.safeParse(p.MAIN, ".a(::.large.type)\n .a.component.$q(::.small.type)\n .a.component.$q.name.$w(::.String)")))

    def test_typing_nested_types_fail(self):
        the_string = """
        ::.String: END
        ::.Number: END

        ::.small.type:
            .name.$x(::.String)
        END

        ::.large.type:
           .component.$x(::.small.type)
        END

        .a(::.large.type).component.$q.name.$w
        """
        result = p.safeParse(p.MAIN, the_string)
        env = Environment(result[:])
        env.validate()
        with self.assertRaises(te.TypeConflictException):
            self.assertTrue(env.query(p.safeParse(p.MAIN, ".a(::.large.type)\n .a.component.$q(::.small.type)\n .a.component.$q.name.$w(::.Number)")))

    def test_typing_polytype(self):
        the_string = """
        ::.String: END
        ::.Number: END

        ::.polytype[$x]:
            .name.$x
        END

        .a(::.polytype(::.String)).name.$q
        .b(::.polytype(::.Number)).name.$t
        """
        result = p.safeParse(p.MAIN, the_string)
        env = Environment(result[:])
        env.validate()
        self.assertTrue(env.query(p.safeParse(p.MAIN, ".a(::.polytype)")))
        self.assertTrue(env.query(p.safeParse(p.MAIN, ".a(::.polytype(::.String))")))
        self.assertTrue(env.query(p.safeParse(p.MAIN, ".a.name.$q(::.String)")))

        self.assertTrue(env.query(p.safeParse(p.MAIN, ".b(::.polytype)")))
        self.assertTrue(env.query(p.safeParse(p.MAIN, ".b(::.polytype(::.Number))")))
        self.assertTrue(env.query(p.safeParse(p.MAIN, ".b.name.$t(::.Number)")))

    def test_typing_polytype_nested(self):
        the_string = """
        ::.String: END
        ::.Number: END

        ::.ptypeOne[$x]:
            .place.$x
        END

        ::.ptypeTwo[$y]:
            .nested(::.ptypeOne(::.$y))
        END

        .a(::.ptypeTwo(::.String)).nested.place.$x
        """

        result = p.safeParse(p.MAIN, the_string)
        env = Environment(result[:])
        env.validate()
        # self.assertTrue(env.query(p.safeParse(p.MAIN, ".a(::.ptypeTwo)")))
        self.assertTrue(env.query(p.safeParse(p.MAIN, ".a(::.ptypeTwo(::.String))")))
        self.assertTrue(env.query(p.safeParse(p.MAIN, ".a.nested.place.$x(::.String)")))

    def test_typing_polytype_nested_2(self):
        the_string = """
        ::.String: END
        ::.Number: END

        ::.ptypeOne[$x]:
            .place.$x
        END

        ::.ptypeTwo[$x]:
            .nested(::.$x)
        END

        .a(::.ptypeTwo(::.ptypeOne(::.Number))).nested.place.$x
        """
        result = p.safeParse(p.MAIN, the_string)
        env = Environment(result[:])
        env.validate()
        # self.assertTrue(env.query(p.safeParse(p.MAIN, ".a(::.ptypeTwo)")))
        self.assertTrue(env.query(p.safeParse(p.MAIN, ".a(::.ptypeTwo(::.ptypeOne(::.Number)))")))
        self.assertTrue(env.query(p.safeParse(p.MAIN, ".a.nested.place.$x(::.Number)")))

    def test_typing_polytype_nested_3(self):
        the_string = """
        ::.String: END
        ::.Number: END

        ::.ptypeOne[$x, $y]:
            .place.$x
            .age.$y
        END

        .a(::.ptypeOne(::.String, ::.Number)).place.$x
        .a.age.$y
        """
        result = p.safeParse(p.MAIN, the_string)
        env = Environment(result[:])
        env.validate()
        # self.assertTrue(env.query(p.safeParse(p.MAIN, ".a(::.ptypeTwo)")))
        self.assertTrue(env.query(p.safeParse(p.MAIN, ".a(::.ptypeOne(::.String, ::.Number))")))
        self.assertTrue(env.query(p.safeParse(p.MAIN, ".a.place.$x(::.String)")))
        self.assertTrue(env.query(p.safeParse(p.MAIN, ".a.age.$y(::.Number)")))

    def test_typing_polytype_nested_fail(self):
        the_string = """
        ::.String: END
        ::.Number: END

        ::.ptypeOne[$x, $y]:
            .place.$x
            .age.$y
        END

        .a(::.ptypeOne(::.String, ::.Number)).place.$x
        .a.age.$x
        """
        result = p.safeParse(p.MAIN, the_string)
        env = Environment(result[:])
        with self.assertRaises(te.TypeConflictException):
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
