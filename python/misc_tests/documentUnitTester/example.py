"""
An Example use of the DocTester module
"""
from doctester import Document, DocTestRunner, DocException, SizedShould

# Setup root_logger:
import logging as root_logger
LOGLEVEL = root_logger.DEBUG
LOG_FILE_NAME = "DocTestExample.log"
root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')

console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)
##############################

class ExampleTester(DocTestRunner):

    def __init__(self):
        super().__init__()
        self.d = Document('./data')

    def test_success_(self):
        return True

    def test_fail_(self):
        raise DocException('Should Fail')

    def test_chapters(self):
        self.d.should.have.chapter('test')
        self.d.should.have.chapter('second')

    def test_fail_chapters(self):
        self.d.should.have.chapter('third')

    def test_section(self):
        self.d.chapter('test').should.have.section('introduction')
        self.d.chapter('second').should.have.section('background')

    def test_fail_section(self):
        self.d.chapter('test').should.have.section('blahh')

    def test_mention(self):
        self.d.chapter('test').should.mention('blah')

    def test_fail_mention(self):
        self.d.chapter('test').should.mention('bloo')

    def test_citation(self):
        self.d.chapter('test').should.cite('Graeber 99')

    def test_fail_mention(self):
        self.d.chapter('test').should.mention('bloo')

    def test_length_pages(self):
        SizedShould.WordsInAPage = 50
        self.d.should.have.length.larger.than(2).pages()

    def test_length_paragraphs(self):
        self.d.chapter('test').should.have.length.larger.than(5).paragraphs()

    def test_length_least(self):
        self.d.chapter('test').section('introduction').should.have.length.at.least(1).paragraphs()

    def test_length_most(self):
        self.d.chapter('test').section('introduction').should.have.length.at.most(4).paragraphs()

    def test_fail_length_paragraphs(self):
        self.d.chapter('second').should.have.length.larger.than(15).paragraphs()

    def test_fail_length_least_paragraphs(self):
        self.d.chapter('second').section('introduction').should.have.length.at.least(10).paragraphs()

    def test_precedence(self):
        self.d.chapter('test').section('introduction').should.precede('conclusion')

    def test_fail_precedence(self):
        self.d.chapter('test').section('conclusion').should.precede('introduction')

    def test_length_larger(self):
        self.d.should.have.length.larger.than(10).sentences()

    def test_fail_length_greater(self):
        self.d.should.have.length.larger.than(100).sentences()

    def test_has_subsections(self):
        self.d.chapter('test').should.have.subsections(2)

    def test_fail_has_subsections(self):
        self.d.chapter('test').should.have.subsections(5)

    def test_fail_citation(self):
        self.d.chapter('test').should.cite('McCoy 2003')


##############################
if __name__ == '__main__':
    runner = ExampleTester()
    runner()
