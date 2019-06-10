import logging as root_logger
logging = root_logger.getLogger(__name__)

class BasicNode:

    def __init__(self):
        self.name = "__root"
        self._type = None

    def __repr__(self):
        return "(root)"

    def is_var(self):
        return False
