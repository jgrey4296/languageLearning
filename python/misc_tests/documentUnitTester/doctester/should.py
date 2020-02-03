"""
	Defines Should Variants that enable chaining assertion syntax
"""
#pylint: disable=no-self-use
import IPython
import logging as root_logger
import regex
from doctester.doc_exception import DocException
logging = root_logger.getLogger(__name__)


class Should:
    """ Should is a stateful chain of tests that when it passes is silent,
    and when it fails raises a DocException """
    def __init__(self, ref):
        self._ref = ref
        self.state = {}

    def __getattr__(self, value):
        non_terminals = "have a near come after use at".split(r' ')
        if value in non_terminals:
            #non-terminals just return the should again for chaining
            return self
        elif value == 'length':
            return self._length()
        elif value == 'subsections':
            return self._subsections
        elif value == 'least':
            return self._length().least
        else:
            raise AttributeError('{} not suitable in a should'.format(value))

    #Terminals:
    def mention(self, reference):
        """ Check for a specific string in the specified section """
        if self._ref.mentions(reference):
            return self
        else:
            raise DocException("No mention found", missing=reference)

    def cite(self, citation):
        """ Check for a citation in the section's citation set """
        citation_set = self._ref.get_citations()
        if citation in citation_set:
            return self
        else:
            raise DocException('No Citation Found: {}'.format(citation))

    def precede(self, name):
        """ Set state for a final test """
        #go up to parent section, ensure the ref section is before the input name
        parent = self._ref.get_parent()
        #Not that elegant, but avoids recursive imports:
        if parent.is_document():
            raise Exception('Attempting to specify order of chapter')
        ordered_titles = [x._title for x in parent._ordered_subsections]
        try:
            index_1 = ordered_titles.index(self._ref._title)
        except ValueError as err:
            raise DocException('Section can not be found', missing=self._ref._title)
        try:
            index_2 = ordered_titles.index(name.lower().strip())
        except ValueError as err:
            raise DocException('Section can not be found', missing=name)



        if index_1 < index_2:
            return self

        err_msg = "Bad Precedence order: {}({}) <-> {}({})"
        raise DocException(err_msg.format(self._ref._title, index_1,
                                          name, index_2))

    def section(self, name):
        """ Test for a section, and set state to allow further chaining """
        return self._ref.section(name)


    def _subsections(self, vals):
        """ Boolean check for correct names, or corrent number, of subsections """
        if isinstance(vals, list) and all([self._ref.section(x) for x in vals]):
            return self
        elif isinstance(vals, int) and len(self._ref._ordered_subsections) == vals:
            return self
        raise DocException('Subsection mismatch: {} / {}'.format(vals,
                                                                 len(self._ref._ordered_subsections)))

    def chapter(self, name):
        """ Test for a chapter, and set the state for further chaining """
        return self._ref.chapter(name)

    def sections(self, *args):
        """ Utility to test for multiple sections """
        for x in args:
            self.section(x)
        return self

    def tag(self, tag):
        """ Test a selected Document/Section/Subsection/Paragraph/Sentence for a tag """
        if not self._ref.has_tag(tag):
            raise DocException("Tag not found", missing=tag)
        return self

    def regex(self, reg):
        """ Test a selected Doc/Sec/SubSec/Para/Sentence for a regex """
        #apply it to the text
        found = False
        if self._ref.is_document():
            for x in self.ref._chapters.values():
                found = found or x.regex(reg)
        #apply it to the subsections
        if self._ref.is_section():
            found = found or self._ref.regex(reg)

        if not found:
            raise DocException('Regex not found')
        return self

    def _length(self):
        # _length instead of length to not interfere with getattr above
        return SizedShould(self._ref)


class SizedShould(Should):
    """
    A Should Variant that enables comparisons of numeric values
    """
    #Mod this as necessary
    WordsInAPage = 500

    def __getattr__(self, value):
        non_terminals = "than at".split(r' ')
        if value in non_terminals:
            #non-terminals just return the should again for chaining
            return self
        elif value == 'larger':
            return self._larger()
        elif value == 'smaller':
            return self._smaller()
        elif value == 'equal':
            return self._equal()
        elif value == 'subsections':
            return self._subsections_len()
        else:
            raise AttributeError('{} not suitable in a should'.format(value))


    def _larger(self):
        self.state['comp'] = lambda a, b: a > b
        return self

    def _smaller(self):
        self.state['comp'] = lambda a, b: a < b
        return self

    def equal(self):
        """ Test if the comparison object equals the one passed in """
        self.state['comp'] = lambda a, b: a == b
        return self

    def least(self, value):
        """ Test if the comparison object is greater than the value"""
        self.state['comp'] = lambda a, b: a >= b
        self.state['compVal'] = value
        return self

    def most(self, value):
        """ Test if the comparison object is less than the value """
        self.state['comp'] = lambda a, b: a <= b
        self.state['compVal'] = value
        return self


    def than(self, value):
        """ Assign a value to be compared against """
        #although it comes first in the chain x.should.have.length.larger.than.pages
        #actually sets up the b value in the comparison
        self.state['compVal'] = value
        return self

    #The things that can be checked for size:
    def pages(self):
        """ Test against a given page count  """
        base_wordcount = self._ref.get_word_count()
        compare_to = SizedShould.WordsInAPage * self.state['compVal']
        if self.state['comp'](base_wordcount, compare_to):
            return self
        else:
            err_msg = 'Not Enough words to fulfill page count: {}/({} * {})'
            raise DocException(err_msg.format(base_wordcount,
                                              self.state['compVal'],
                                              SizedShould.WordsInAPage))

    def paragraphs(self):
        """ Test against a given paragraph count """
        paragraph_count = self._ref.get_paragraph_count()
        if self.state['comp'](paragraph_count, self.state['compVal']):
            return self
        else:
            err_msg = "Not Enough Paragraphs: {} / {}"
            raise DocException(err_msg.format(paragraph_count,
                                              self.state['compVal']))

    def sentences(self):
        """ Test against a given sentence count """
        sentence_count = self._ref.get_sentence_count()
        if self.state['comp'](sentence_count, self.state['compVal']):
            return self
        else:
            err_msg = 'Not Enough Sentences: {} / {}'
            raise DocException(err_msg.format(sentence_count,
                                              self.state['compVal']))

    def words(self):
        """ Test against a given word count """
        word_count = self._ref.get_word_count()
        if self.state['comp'](word_count, self.state['compVal']):
            return self
        else:
            raise DocException('Not Enough Words: {} / {}'.format(word_count,
                                                                  self.state['compVal']))

    def citations(self):
        """ Test against a given citation count """
        cite_count = len(self._ref.get_citations())
        if self.state['comp'](cite_count, self.state['compVal']):
            return self
        else:
            err_msg = 'Not Enough Citations: {} / {}'
            raise DocException(err_msg.format(cite_count,
                                              self.state['compVal']))

    def _subsections_len(self):
        """ Test against a given subsection count """
        num_sections = len(self._ref.subsections)
        if self.state['comp'](num_sections, self.state['compVal']):
            return self
        else:
            err_msg = 'Not Enough Subsections: {} / {}'
            raise DocException(err_msg.format(num_sections,
                                              self.state['compVal']))
