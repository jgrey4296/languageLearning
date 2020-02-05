"""
	The Main Section class to hold text, references and tags
"""
#pylint: disable=no-self-use
import logging as root_logger
import spacy
import IPython
import regex
from doctester.doc_exception import DocException
from doctester.should import Should

logging = root_logger.getLogger(__name__)
NLP = spacy.load('en')


class Section:
    """ An individual section of a document """
    def __init__(self, title, level):
        #the name of the section / chapter
        self._title = title
        #Any extracted tags and sections from the text
        self._level = level
        self._parent_section = None
        #paragraphs hold tags and citations fields
        self._paragraphs = []
        self._tags = set()
        self._ordered_subsections = []
        self._named_subsections = {}

    def __getattr__(self, value):
        """ Overrides the default getattr to allow something.should,
        while not breaking the typical usage of getattr """
        if value == 'should':
            return Should(self)
        else:
            raise AttributeError('{} not suitable for Section'.format(value))

    def is_section(self):
        """ is this object a section"""
        return True

    def is_document(self):
        """ is this object a document"""
        return False

    def sections(self):
        """ Get the subections of this object"""
        return self._ordered_subsections.copy()

    def title_is(self, a):
        """ Is this title of this object a? """
        return self._title == a

    def section(self, value):
        """ Get a subsection of a section """
        fvalue = value.lower().strip()
        if fvalue in self._named_subsections:
            return self._named_subsections[fvalue]
        else:
            raise DocException("Subsection not found", missing=value)

    def add_subsection(self, title, level):
        """ Add a new subsection to the current section, or a set indentation level """
        ftitle = title.lower().strip()
        new_section = Section(ftitle, level)
        new_section.set_parent(self)
        self._ordered_subsections.append(new_section)
        self._named_subsections[ftitle] = new_section
        return new_section

    def get_parent(self):
        """ Get the parent section of this object """
        return self._parent_section

    def set_parent(self, ref):
        """ Assign an object to be the parent of this section """
        if self._parent_section is not None:
            raise Exception('Attempting to redefine parent section')
        elif ref.is_section() and ref._level >= self._level:
            raise Exception('Attempting to set a parent that is of a bad level')
        else:
            self._parent_section = ref

    def add_tag(self, text):
        """ Add a descriptive tag to this section"""
        ftext = text.lower().strip()
        self._tags.add(ftext)
        return self

    def has_tag(self, text):
        """ Does this object have a particular tag? """
        ftext = text.lower().strip()
        if ftext in self._tags:
            return True
        if any([ftext in p['tags'] for p in self._paragraphs]):
            return True
        else:
            return any([sub.has_tag(ftext) for sub in self._ordered_subsections])

    def has_citation(self, text):
        """ Does this object have a specific citation """
        ftext = text.lower().strip()
        return any([ftext in p['citations'] for p in self._paragraphs]) \
            or any([sub.has_citation(ftext) for sub in self._ordered_subsections])


    def add_paragraph(self, text):
        """ Add a new paragraph to this section """
        new_paragraph = {'text':NLP(text), 'tags': set(), 'citations': set()}
        self._paragraphs.append(new_paragraph)
        return new_paragraph

    def get_paragraphs(self):
        """ Get the paragraphs of this section"""
        return self._paragraphs

    def get_all_paragraphs(self):
        """ Get all the paragraphs of all the subsections of this object """
        initial = self._paragraphs.copy()
        initial.extend([x for ss in self._ordered_subsections for x in ss.get_all_paragraphs()])
        return initial


    def get_sentence_count(self):
        """ Get the total sentence count of this sections paragraphs,
        and sub-sections sentence counts """
        base_count = 0
        base_count += sum([len(list(x['text'].sents)) for x in self._paragraphs])
        base_count += sum([x.get_sentence_count() for x in self._ordered_subsections])
        return base_count

    def get_word_count(self):
        """ Get the total word count of paragraphs + subsections """
        base_count = 0
        for paragraph in self._paragraphs:
            words = [token for token in paragraph['text'] \
                     if token.is_punct is False]
            base_count += len(words)
        base_count += sum([x.get_word_count() for x in self._ordered_subsections])
        return base_count

    def get_paragraph_count(self):
        """ Get the total paragraph count of this section + subsections """
        base_count = len(self._paragraphs)
        base_count += sum([x.get_paragraph_count() for x in self._ordered_subsections])
        return base_count

    def get_citations(self):
        """ Get the union set of all citations in paragraphs + subsections """
        base_set = set()
        paragraph_sets = [x['citations'] for x in self._paragraphs]
        subsection_sets = [x.get_citations() for x in self._ordered_subsections]
        base_set = base_set.union(*paragraph_sets, *subsection_sets)
        return base_set

    def mentions(self, reference):
        """ Check for a string literal in the text of the section + subsections """
        for paragraph in self._paragraphs:
            if reference in paragraph['text'].text:
                return True
        for section in self._ordered_subsections:
            if section.mentions(reference):
                return True

        return False

    def regex(self, reg):
        """ Test the text of the section and below for a regex """
        re = regex.compile(reg)
        for paragraph in self._paragraphs:
            if bool(re.search(paragraph['text'].text)):
                return True
        for section in self._ordered_subsections:
            if section.regex(reg):
                return True
        return False
