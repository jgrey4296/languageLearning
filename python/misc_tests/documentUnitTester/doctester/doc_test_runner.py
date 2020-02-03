"""
Module Defines the Base Class for Automating Document Tests
"""
#pylint: disable=too-few-public-methods
import logging as root_logger
from doctester.doc_exception import DocException
logging = root_logger.getLogger(__name__)

class DocTestRunner:
    """ The main test runner class. Subclass this to add tests """
    #colours from stackoverflow q 287871
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAILC = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

    #Useful constants:
    TEST_NAME = 'test_'
    FAIL = 'test_fail_'
    TICK = OKGREEN + '✓' + ENDC
    CROSS = FAILC + 'X' + ENDC
    TAB = "	"

    def __call__(self):
        """ The main call to run tests """
        passed = 0
        failed = 0
        tests = [x for x in dir(self) if DocTestRunner.TEST_NAME in x and \
                 DocTestRunner.FAIL not in x]
        fail_tests = [x for x in dir(self) if DocTestRunner.FAIL in x]
        total = len(tests)+len(fail_tests)
        logging.info('====================')
        logging.info("Running Tests:")
        logging.info('====================')
        for test in tests:
            try:
                result = getattr(self, test)()
                if result is not None and result is False:
                    raise DocException('Test returned False')
            except DocException as err:
                logging.warning('------------------------------')
                logging.warning("{}{} {} : {} {} {} {} {}".format(DocTestRunner.TAB,
                                                                  DocTestRunner.TAB,
                                                                  DocTestRunner.CROSS,
                                                                  test,
                                                                  '\n',
                                                                  DocTestRunner.TAB,
                                                                  DocTestRunner.TAB,
                                                                  repr(err)))
                logging.warning('------------------------------')
                failed += 1
            else:
                logging.info("{} {} : {}".format(DocTestRunner.TAB,
                                                 DocTestRunner.TICK,
                                                 test))
                passed += 1

        logging.info('====================')
        logging.info('Running Fail Tests:')
        logging.info('====================')
        for test in fail_tests:
            try:
                result = getattr(self, test)()
                if result is None or result is False:
                    logging.info('------------------------------')
                    logging.info('{}{} {} : {}'.format(DocTestRunner.TAB,
                                                       DocTestRunner.TAB,
                                                       DocTestRunner.CROSS,
                                                       test))
                    logging.info('------------------------------')
                    failed += 1
            except DocException as err:
                passed += 1
                logging.info("{} {} : {}".format(DocTestRunner.TAB,
                                                 DocTestRunner.TICK,
                                                 test))

        log_str = "\n\nFinished. {}/{} Passed. {}/{} Failed."
        logging.info(log_str.format(passed, total, failed, total))
