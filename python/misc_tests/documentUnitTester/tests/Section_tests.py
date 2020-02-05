import unittest
import logging
from test_context import doctester as dt
from doctester.section import Section
from doctester.should import Should

class Section_Tests(unittest.TestCase):

    #use testcase snippets
    def test_initialise(self):
        aSection = Section('aTitle', 1)
        self.assertIsInstance(aSection, Section)
        self.assertEqual(aSection._title, 'aTitle')
        self.assertEqual(aSection._level, 1)
        self.assertTrue(aSection.is_section())
        self.assertFalse(aSection.is_document())


    def test_should_retrieval(self):
        aSection = Section('aTitle', 1)
        self.assertIsInstance(aSection.should, Should)
        self.assertEqual(aSection.should._ref, aSection)

    def test_set_parent(self):
        aSection = Section('aTitle', 1)
        anotherSection = Section('AnotherTitle', 2)
        self.assertIsNone(anotherSection.get_parent())
        anotherSection.set_parent(aSection)
        self.assertIsNotNone(anotherSection.get_parent())

    def test_bad_set_parent(self):
        aSection = Section('aTitle', 1)
        anotherSection = Section('AnotherTitle', 2)
        self.assertIsNone(aSection.get_parent())
        with self.assertRaises(Exception):
            aSection.set_parent(anotherSection)

    def test_tags(self):
        aSection = Section('aTitle', 1)
        aSection.add_tag('blah').add_tag('bloo')
        self.assertTrue(aSection.has_tag('blah'))
        self.assertTrue(aSection.has_tag('bloo'))
        self.assertFalse(aSection.has_tag('blee'))

    def test_paragraphs(self):
        aSection = Section('aTitle', 1)
        self.assertEqual(len(aSection._paragraphs), 0)
        aSection.add_paragraph("this is a test text.")
        self.assertEqual(len(aSection._paragraphs), 1)
        aSection.add_paragraph("this is another test text.")
        self.assertEqual(len(aSection._paragraphs), 2)
        paragraphs = aSection.get_paragraphs()
        self.assertEqual(len(paragraphs), 2)
        self.assertEqual(paragraphs[0]['text'].text, "this is a test text.")
        self.assertEqual(paragraphs[1]['text'].text, "this is another test text.")

    def test_subsection(self):
        aSection = Section('aTitle', 1)
        newSection = aSection.add_subsection('other', 2)
        self.assertEqual(len(aSection._ordered_subsections), 1)
        retrieved = aSection.section('other')
        self.assertIsNotNone(retrieved)
        self.assertIsInstance(retrieved, Section)

    def test_subsection_bad_level(self):
        aSection = Section('aTitle', 1)
        with self.assertRaises(Exception):
            aSection.add_subsection('other', 1)

    def test_get_all_paragraphs(self):
        aSection = Section('aTitle', 1)
        subSection = aSection.add_subsection('other', 2)
        subSection.add_paragraph('This is a sub section paragraph')
        paragraphs = aSection.get_all_paragraphs()
        self.assertEqual(len(paragraphs), 1)
        self.assertEqual(paragraphs[0]['text'].text, 'This is a sub section paragraph')

    def test_local_sentence_count(self):
        aSection = Section('aTitle', 1)
        aSection.add_paragraph('This is a single sentence.')
        self.assertEqual(aSection.get_sentence_count(), 1)
        aSection.add_paragraph('This adds a second, and third, sentence. How is the weather today?')
        self.assertEqual(aSection.get_sentence_count(), 3)

    def test_deep_sentence_count(self):
        aSection = Section('aTitle', 1)
        subSection = aSection.add_subsection('other', 2)
        aSection.add_paragraph('This is the first sections paragraph. And a second sentence.')
        subSection.add_paragraph('This is a sub section paragraph. The weather today is fairly nice.')
        self.assertEqual(aSection.get_sentence_count(), 4)

    def test_deep_word_count(self):
        aSection = Section('aTitle', 1)
        subSection = aSection.add_subsection('other', 2)
        aSection.add_paragraph('This is the word sections paragraph. And a second sentence.')
        subSection.add_paragraph('This is a sub word paragraph. The weather today is fairly nice.')
        self.assertEqual(aSection.get_word_count(), 22)

    def test_tags(self):
        aSection = Section('aTitle', 1)
        aSection.add_tag('blah')
        self.assertTrue(aSection.has_tag('blah'))
        self.assertFalse(aSection.has_tag('bloo'))

    def test_deep_tags_paragraphs(self):
        aSection = Section('aTitle', 1)
        aParagraph = aSection.add_paragraph('This is some random text')
        self.assertIn('text', aParagraph)
        self.assertIn('tags', aParagraph)
        self.assertIn('citations', aParagraph)
        aParagraph['tags'].add('atag')
        self.assertIn('atag', aParagraph['tags'])
        self.assertTrue(aSection.has_tag('aTag'))
        self.assertFalse(aSection.has_tag('bloo'))

    def test_deep_tags_sections(self):
        aSection = Section('aTitle', 1)
        aSubSection = aSection.add_subsection('subsection', 2)
        aSubSection.add_tag('blah')
        self.assertTrue(aSection.has_tag('blah'))

    def test_citations_paragraphs(self):
        aSection = Section('aTitle', 1)
        aParagraph = aSection.add_paragraph('this is some text')
        aParagraph['citations'].add('graeber 99')
        self.assertTrue(aSection.has_citation('graeber 99'))

if __name__ == "__main__":
      LOGLEVEL = logging.DEBUG
      logFileName = "log.section_tests"
      logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
      console = logging.StreamHandler()
      console.setLevel(logging.INFO)
      logging.getLogger('').addHandler(console)
      unittest.main()
