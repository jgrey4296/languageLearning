import unittest
import logging
from test_context import doctester as dt
from doctester.document import Document
from doctester.section import Section
from doctester.should import Should
from doctester.doc_exception import DocException

class Should_Tests(unittest.TestCase):

    def setUp(self):
        self.doc = Document('./data')

    def tearDown(self):
        self.doc = None

    def test_existence(self):
        #Check should class exists,
        #and exists from appropriate creations
        self.assertIsNotNone(Should)

    def test_doc_creation(self):
        shd = self.doc.should
        self.assertIsNotNone(shd)
        self.assertIsInstance(shd, Should)

    def test_section_creation(self):
        sec = self.doc.chapter('test')
        self.assertIsInstance(sec, Section)
        self.assertIsInstance(sec.should, Should)

    def test_mention(self):
        #Check mention call
        self.assertTrue(self.doc.should.mention('graeber'))

    def test_cite(self):
        #check cite call
        self.assertTrue(self.doc.should.cite('Graeber 99'))

    def test_precede(self):
        #check precede call
        chap = self.doc.chapter('test')
        self.assertTrue(chap.section('introduction').should.precede('conclusion'))

    def test_section(self):
        #check contains section call
        chap = self.doc.chapter('test')
        self.assertTrue(chap.should.have.section('introduction'))

    def test_subsections(self):
        #check contains subsections call
        self.assertTrue(True)

    def test_chapter(self):
        #check contains chapter call
        self.assertTrue(self.doc.should.have.chapter('test'))
        with self.assertRaises(DocException):
            self.doc.should.have.chapter('blah')

    def test_sections(self):
        #check contains multiple sections call
        self.assertTrue(self.doc.chapter('test').should.have.sections('introduction','conclusion'))

    def test_tag(self):
        #Check tag call
        self.assertTrue(self.doc.chapter('test').should.have.tag('intro'))

    def test_regex(self):
        #check regex test call
        self.assertTrue(self.doc.chapter('test').should.have.regex(r'the'))

class SizedShould_Tests(unittest.TestCase):

    def setUp(self):
        self.doc = Document('./data')

    def tearDown(self):
        self.doc = None

    def test_larger(self):
        self.assertTrue(self.doc.should.have.length.larger.than(10).sentences())

    def test_smaller(self):
        self.assertTrue(self.doc.should.have.length.smaller.than(50).sentences())

    def test_equal(self):
        self.assertTrue(True)

    def test_least(self):
        self.assertTrue(True)

    def test_most(self):
        self.assertTrue(True)

    def test_than(self):
        self.assertTrue(True)

    def test_pages(self):
        self.assertTrue(True)

    def test_sentences(self):
        self.assertTrue(True)

    def test_words(self):
        self.assertTrue(True)

    def test_citations(self):
        self.assertTrue(True)

    def test_subsections_len(self):
        self.assertTrue(True)



if __name__ == "__main__":
      LOGLEVEL = logging.DEBUG
      logFileName = "doctester_tests.log"
      logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
      console = logging.StreamHandler()
      console.setLevel(logging.INFO)
      logging.getLogger('').addHandler(console)
      unittest.main()
