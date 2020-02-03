import unittest
import logging
import IPython
from test_context import doctester as dt
from doctester.document import Document
from doctester.section import Section
from doctester.should import Should
from doctester.doc_exception import DocException

class Document_Tests(unittest.TestCase):

    def setUp(self):
        self.doc = Document('./data')

    def test_existence(self):
        self.assertIsInstance(self.doc,Document)
        self.assertFalse(self.doc.is_section())
        self.assertTrue(self.doc.is_document())

    def test_found_files(self):
        self.assertEqual(self.doc._directory, './data')
        self.assertTrue('test.org' in self.doc._files)
        self.assertTrue('second.org' in self.doc._files)

    def test_chapter(self):
        self.assertIsNotNone(self.doc.chapter('test'))
        with self.assertRaises(DocException):
            self.doc.chapter('blah')

    def test_word_count(self):
        self.assertGreater(self.doc.get_word_count(), 30)

    def test_sentence_count(self):
        self.assertGreater(self.doc.get_sentence_count(), 10)

    def test_citations(self):
        self.assertGreater(len(self.doc.get_citations()), 0)

    def test_mentions(self):
        self.assertTrue(self.doc.mentions('cites'))
        self.assertTrue(self.doc.mentions('graeber'))
        self.assertFalse(self.doc.mentions('queen'))
        self.assertTrue(self.doc.mentions('additional'))

if __name__ == "__main__":
      LOGLEVEL = logging.DEBUG
      logFileName = "log.doctester_tests"
      logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
      console = logging.StreamHandler()
      console.setLevel(logging.INFO)
      logging.getLogger('').addHandler(console)
      unittest.main()
