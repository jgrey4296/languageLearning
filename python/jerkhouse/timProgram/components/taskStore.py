import logging
from os import listdir
from os.path import isfile,join
from functools import partial
from collections import namedtuple
from datetime import datetime, timedelta
import pickle
from enum import Enum,IntEnum
import IPython

TITLE_DELIM = '*'
DAY = 'day'
REPEAT = 'repeat'
NOTES = 'notes'
PERFORMANCE_FILE = ".performances"
DATE_FORMAT = "%d/%m/%Y"
REPEAT_TYPES = Enum('Repeat_Types','Immediately Daily Weekly Bimonthly Monthly Biannually Annually')
REPEAT_DELTAS = {
    REPEAT_TYPES.Immediately : timedelta(minutes=1),
    REPEAT_TYPES.Daily : timedelta(days=1),
    REPEAT_TYPES.Weekly : timedelta(weeks=1),
    REPEAT_TYPES.Bimonthly : timedelta(weeks=2),
    REPEAT_TYPES.Monthly : timedelta(weeks=4),
    REPEAT_TYPES.Biannually : timedelta(weeks=6*4),
    REPEAT_TYPES.Annually : timedelta(weeks=12*4)
}

SINGLEDAY = timedelta(days=1)
DAYS = IntEnum('Days','mon tue wed thur fri sat sun')

Task = namedtuple('Task','name day repeat text originalFile marked')

class TaskStore:

    def __init__(self,dataDir,task_const):
        logging.debug("Creating Task Store")
        self.data_directory = dataDir
        self.task_const = task_const
        #Dictionary of name -> Tasks
        self.tasks = {}
        self.task_performance_times = None
        self.critical_tasks = set()
        self.dayDifference = {}
        self.loadTasks()
        self.load_task_performance_times()
        self.verify_task_performance_times()

    def update_task(self,taskName):
        """ Given a task name, mark it as performed at the current datetime  """
        if not taskName in self.tasks:
            raise Exception("Trying to update a task that doesnt exist")
        logging.info("Updating: {}".format(taskName))
        now = datetime.now()
        self.task_performance_times[taskName] = now

    def load_task_performance_times(self):
        """ Load the additional performance time file """
        if self.task_performance_times is not None:
            raise Exception("Trying to load task times when already loaded")
        try:
            with open(join(self.data_directory,PERFORMANCE_FILE),'rb') as f:
                self.task_performance_times = pickle.load(f)
        except FileNotFoundError as e:
            logging.debug("No performance log, adding empty log")
            self.task_performance_times = {}
            
    def save_task_performance_times(self):
        """ Save a task and the time it was performed   """
        logging.debug("Saving performance times")
        if self.task_performance_times is None:
            logging.debug("No performance times to save")
            return
        with open(join(self.data_directory,PERFORMANCE_FILE),'wb') as f:
            pickle.dump(self.task_performance_times,f)

    def verify_task_performance_times(self):
        """ Check the performance time of each task, to how recently it should
            have been performed
        """
        now = datetime.now()
        for task,date_performed in self.task_performance_times.items():
            logging.debug("Verifying {}".format(task))
            try:
                repeatType = REPEAT_DELTAS[REPEAT_TYPES[self.tasks[task].repeat]]
            except KeyError as e:
                logging.exception("Unrecognised Repeat: {}".format(self.tasks[task].repeat))
                repeatType = REPEAT_DELTAS[REPEAT_TYPES.Daily]
            min_target = now - repeatType
            #compare the stored time to the current time and the repeat value,
            #making sure the task has occurred more recently than its repeat value
            if date_performed < min_target and self.dayDifference[task] == 0:
                logging.debug("Task: {} is critical".format(task))
                self.critical_tasks.add(task)
            elif task in self.critical_tasks:
                logging.debug("Removing task: {}".format(task))
                self.critical_tasks.remove(task)
            else:
                logging.debug("Verified task: {}".format(task))
    
    def getTaskNames(self):
        return self.tasks.keys()

    def getTaskPerformanceDate(self,taskName):
        """ Return a string of when a task was performed last, and a boolean of if that
            length of time is critical
        """
        if not taskName in self.tasks:
            raise Exception("Unrecognised taskname queried for performance date: {}".format(taskName))
        if not taskName in self.task_performance_times:
            return ("N/A",False)
        isCritical = taskName in self.critical_tasks
        return (self.task_performance_times[taskName].strftime(DATE_FORMAT),isCritical)
    
    def getTask(self,name):
        if not name in self.tasks:
            raise Exception("Task not found: {}".format(name))
        return self.tasks[name]
    
    def loadTasks(self):
        logging.debug("Loading Tasks from: {} of type: {}".format(self.data_directory,self.task_const))
        try:
            #get all tasks:
            #TODO: Exclude all backup files
            tasks = [f for f in listdir(self.data_directory) if isfile(join(self.data_directory,f)) and \
                       self.task_const in f.lower()]
            logging.info("Tasks found: {}".format(tasks))
            #Process all tasks:
            for t in tasks:
                with open(join(self.data_directory,t)) as f:
                    try:
                        data = f.read()
                        name,day,repeat,text,dayDiff = self.process_task(data.splitlines())
                        if name is not None:
                            logging.info("Loading task: {}".format(name))
                            tempName = name
                            i = 0
                            while tempName in self.tasks:
                                i += 1
                                tempName = "{}_{}".format(name,i)
                            markForUpdate = False
                            self.tasks[tempName] = Task(tempName, day,repeat,text,t,markForUpdate)
                            self.dayDifference[tempName] = dayDiff
                    except Exception as e:
                        logging.exception("Task Load Exception: {}".format(e))
        except Exception as e:
            logging.exception("Task Access Exception: {}".format(e))
            
    def process_task(self,lines):
        """ Take the text of a task, extract the name, ingredients, and instructions
            as separate strings
        """
        logging.debug("Processing Task")
        #The Data to extract:
        name = None
        day = None
        repeat = None
        notes = None
        dayDiff = None
        
        filteredLines = [x.strip() for x in lines if x.strip() != '']
        titles = [(i,x) for i,x in enumerate(filteredLines) if x[0] == TITLE_DELIM]
        #Go through each title, and extract information as appropriate:
        for i,(line,title) in enumerate(titles):
            if i+1 < len(titles):
                sectionEnd = titles[i+1][0]
            else:
                sectionEnd = len(filteredLines)
            #----
            logging.debug("Going to slice {}-{} for {}".format(line,sectionEnd,title))
            #TODO:
            if DAY in title.lower():
                try:
                    dayStr = filteredLines[line+1].strip().lower()
                    dayValue = DAYS[dayStr]
                    day = dayValue.name
                    dayDiff = dayDifference(dayValue,datetime.now().weekday())
                except Exception as e:
                    logging.exception("Day difference calculation exception: {}".format(e))
            elif REPEAT in title.lower():
                repeat = "\n".join(filteredLines[line+1:sectionEnd])
            elif NOTES in title.lower():
                notes = "\n".join(filteredLines[line+1:sectionEnd])
            else:
                name = title
        #----
        if name and day and repeat and notes:
            logging.debug("Extracted: {} - {} - {} - {}".format(name,day,repeat,notes))
            return (name,day,repeat,notes,dayDiff)
        else:
            logging.warning("Task processing failure")
            return (None,None,None,None,None)


def dayDifference(target,current):
    return min(abs(target-current),abs(target-current+7))
