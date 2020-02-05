"""
	Defines Exceptions that the Testing Framework can raise
"""

class DocException(Exception):
    """ The exception class thrown by any doc/should test """

    def __init__(self, desc, **kwargs):
        """ Initialise the exception, with open keyword args available """
        super().__init__()
        self.desc = desc
        self.vals = {}
        for key in kwargs:
            self.vals[key] = kwargs[key]

    def __repr__(self):
        """ Based on the keywords used for this exception, make a string repr """
        if 'expected' and 'actual' in self.vals:
            return "{} : Expected {}, Got {}".format(self.desc,
                                                     self.vals['expected'],
                                                     self.vals['actual'])
        elif 'missing' in self.vals:
            return "{} : missing {}".format(self.desc, self.vals['missing'])
        else:
            return "{}".format(self.desc)
