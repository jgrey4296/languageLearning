"""
	Defines the Document class, top level collection of texts
"""
#pylint: disable=no-self-use
import IPython
import logging as root_logger
from os.path import join, isdir, splitext
from os import listdir
from doctester.doc_exception import DocException
from doctester.should import Should
from doctester.text_parser import parseText
logging = root_logger.getLogger(__name__)

class Document:
    """ Top level document, loads files, sorts them into chapters,
    and allows access to 'should' testing """
    FILETYPE = '.org'


    def __init__(self, directory=None):
        """ Given a directory, load in all files of FILETYPE, and create indiv chapters for them """
        if not isdir(directory):
            raise Exception("Bad Directory Specification: {}".format(directory))
        read_files = listdir(directory)
        org_files = [x for x in read_files if splitext(x)[1] == Document.FILETYPE and x[0] != '#']
        logging.debug('Selected files: {}'.format(org_files))
        self._directory = directory
        self._files = org_files
        self._chapters = {}
        #Actually read in all found files
        self.read_files()

    def __getattr__(self, value):
        #allows doc.should, instead of doc.should()
        #while retaining doc.read_files()
        if value == 'should':
            return Should(self)
        else:
            raise AttributeError('{} Not Suitable for Document'.format(value))

    def is_section(self):
        """ Is this object a section """
        return False

    def is_document(self):
        """ Is this object a document """
        return True

    def read_files(self):
        """ Open and parse the files in the document's directory """
        for file in self._files:
            fullpath = join(self._directory, file)
            title = splitext(file)[0]
            with open(fullpath, 'r') as f:
                text = f.read()
            #chapters are the same ds as sections
            try:
                new_chapter = parseText(text)
            except Exception as e:
                logging.info("Issue with: {}".format(file))
                raise e

            new_chapter.set_parent(self)
            self._chapters[title.lower().strip()] = new_chapter

    def chapter(self, name):
        """ Get a chapter from the document, use the same error as 'should'ing if it fails """
        fname = name.lower().strip()
        if fname in self._chapters:
            return self._chapters[fname]
        else:
            raise DocException("No Chapter Found", missing=name)

    def get_word_count(self):
        """ Get the number of words in all the chapters of the document """
        return sum([x.get_word_count() for x in self._chapters.values()])

    def get_sentence_count(self):
        """ Get the number of sentences in the chapters of the document """
        return sum([x.get_sentence_count() for x in self._chapters.values()])

    def get_paragraph_count(self):
        """ Get the number of paragraphs in the chapters of the document """
        return sum([x.get_paragraph_count() for x in self._chapters.values()])

    def get_citations(self):
        """ Get the Union of all citation sets of all sub chapters/sections """
        base_set = set()
        citation_sets = [x.get_citations() for x in self._chapters.values()]
        for chapter in citation_sets:
            base_set = base_set.union(chapter)
        return base_set

    def mentions(self, reference):
        """ Test for a literal string in the documents chapters/sections """
        for chapter in self._chapters.values():
            if chapter.mentions(reference):
                return True
        return False

    def chapters(self):
        """ Get the individual chapters of the document as a list """
        return list(self._chapters.values())
