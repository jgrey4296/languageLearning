import unittest
import logging
import IPython
from test_context import pyAStar
from pyAStar import Grid, AStar

class AStar_Tests(unittest.TestCase):

      def setUp(self):
            return 1

      def tearDown(self):
            return 1

      #----------
      def test_basic(self):
          for x in range(10):
              size = 10
              start = (0,0)
              end = (size-1, size-1)
              grid = Grid(size)
              astar = AStar()
              path = astar(grid, start, end)
              path_str = grid.repr_path(path)
              self.assertEqual(start, path[0])
              self.assertEqual(end, path[-1])



if __name__ == "__main__":
      #use python $filename to use this logging setup
      LOGLEVEL = logging.DEBUG
      logFileName = "log.AStar_Tests"
      logging.basicConfig(filename=logFileName, level=LOGLEVEL, filemode='w')
      console = logging.StreamHandler()
      console.setLevel(logging.WARN)
      logging.getLogger().addHandler(console)
      unittest.main()
      #reminder: user logging.getLogger().setLevel(logging.NOTSET) for log control
