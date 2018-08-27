import tkinter as tk
import tkinter.ttk as ttk
import logging
from os import listdir
from os.path import isfile,join
from functools import partial
import IPython

from components.taskStore import TaskStore,Task

TREEVIEW_ACTION = "<Button-1>"
EMPTY_LABEL_TEXT = "          \n          \n"
DEFAULT_LABEL_SIZE = 15
TASK_NAME_COLUMN = "#0"
TASK_DATE_COLUMN = "#1"

class TaskWindow(tk.Toplevel):
    """ A Window for displaying tasks """

    def __init__(self,root):
        """ Initialise the window, creating a task store that loads tasks from data """
        logging.debug("Creating Task Window")
        tk.Toplevel.__init__(self)
        self.root = root
        self.tasks = TaskStore(self.root.data_dir,self.root.task_const)
        self.create_widgets()
        self.doneButton = None
            
    def create_widgets(self):
        """ Create all the widgets in the window. See design notes for details """
        logging.debug("Creating Task Widgets")
        #Task list on the left, as a tree
        self.taskList = ttk.Treeview(self,columns=2)
        self.taskList.heading(TASK_NAME_COLUMN,text='Task:')
        self.taskList.heading(TASK_DATE_COLUMN,text='Last Performed:')
        self.taskList.pack(side="left")
        #get all the tasks
        theTasks = self.tasks.getTaskNames()
        #add them to the treeview
        for i,taskName in enumerate(sorted(theTasks)):
            logging.info("Making button for: {}".format(taskName))
            #CORE UPDATE ACTION:
            newEntry = self.taskList.insert('',i)
            item = self.taskList.item(newEntry,text=taskName)
            #write the 'done' status column
            lastStoredDate,isCritical = self.tasks.getTaskPerformanceDate(taskName)
            self.set_performance_column(newEntry,lastStoredDate,isCritical)
            
        #bind the update function to the treeview:
        self.taskList.bind(TREEVIEW_ACTION,self.display_task_listener)
                        
        #Separate the Task List from the Task display:
        ttk.Separator(self,orient=tk.VERTICAL).pack(side="left",fill=tk.Y,expand=True)
        
        #Task Frame:
        self.taskFrame = tk.Frame(self)
        self.taskFrame.pack(side="right")

        #Task Title
        self.taskLabel = tk.Label(self.taskFrame,borderwidth=5)
        self.taskLabel.grid(row=0,column=1)

        #separate title from the rest
        ttk.Separator(self.taskFrame,orient=tk.HORIZONTAL).grid(row=1,column=0,columnspan=3,sticky=tk.E+tk.W)

        #CORE DISPLAYS: day, repeat, notes
        self.taskDay = tk.Label(self.taskFrame,width=DEFAULT_LABEL_SIZE,height=DEFAULT_LABEL_SIZE,padx=DEFAULT_LABEL_SIZE,pady=DEFAULT_LABEL_SIZE)
        self.taskRepeat = tk.Label(self.taskFrame,width=DEFAULT_LABEL_SIZE,height=DEFAULT_LABEL_SIZE,padx=DEFAULT_LABEL_SIZE,pady=DEFAULT_LABEL_SIZE)
        self.taskNotes = tk.Label(self.taskFrame,width=DEFAULT_LABEL_SIZE,height=DEFAULT_LABEL_SIZE,padx=DEFAULT_LABEL_SIZE,pady=DEFAULT_LABEL_SIZE)

        dayLabel = tk.Label(self.taskFrame)
        dayLabel['text'] = "DO ON:"
        dayLabel.grid(row=2,column=1)
        ttk.Separator(self.taskFrame,orient=tk.HORIZONTAL).grid(row=3,column=0,columnspan=3,sticky=tk.E+tk.W)       
        self.taskDay.grid(row=4,column=1)

        ttk.Separator(self.taskFrame,orient=tk.HORIZONTAL).grid(row=5,column=0,columnspan=3,sticky=tk.E+tk.W)
        repeatLabel = tk.Label(self.taskFrame)
        repeatLabel['text'] = "REPEAT SCHEDULE:"
        repeatLabel.grid(row=6,column=1)
        ttk.Separator(self.taskFrame,orient=tk.HORIZONTAL).grid(row=7,column=0,columnspan=3,sticky=tk.E+tk.W)

        self.taskRepeat.grid(row=8,column=1)

        ttk.Separator(self.taskFrame,orient=tk.HORIZONTAL).grid(row=9,column=0,columnspan=3,sticky=tk.E+tk.W)
        noteLabel = tk.Label(self.taskFrame)
        noteLabel['text'] = "NOTES:"
        noteLabel.grid(row=10,column=1)
        ttk.Separator(self.taskFrame,orient=tk.HORIZONTAL).grid(row=11,column=0,columnspan=3,sticky=tk.E+tk.W)
        
        self.taskNotes.grid(row=12,column=1)
        self.taskDay['text'] = EMPTY_LABEL_TEXT
        self.taskRepeat['text'] = EMPTY_LABEL_TEXT
        self.taskNotes['text'] = EMPTY_LABEL_TEXT

                
    def display_task_listener(self,event):
        """ The Update method called when a task button is pressed """
        item = self.taskList.identify('item',event.x,event.y)
        logging.debug("Retrieved item: {}".format(item))
        if item == "":
            return
        taskName = self.taskList.item(item,'text')
        task = self.tasks.getTask(taskName)
        
        logging.debug("Displaying Task: {}".format(taskName))
        if not isinstance(task,Task):
            raise Exception("Trying to display something that isnt a Task")
        
        self.taskLabel['text'] = taskName
        self.taskDay['text'] = task.day
        self.taskRepeat['text'] = task.repeat
        self.taskNotes['text'] = task.text

        if self.doneButton is None:
            #The done button:
            self.doneButton = tk.Button(self.taskFrame,command=self.update_task)
            self.doneButton['text'] = "Done"
            self.doneButton.grid(row=0,column=2)
        
            
    def update_task(self):
        currentTaskName = self.taskLabel['text']
        self.tasks.update_task(currentTaskName)
        self.tasks.save_task_performance_times()
        #update all columns in the treeview
        self.update_dates()

        
    def update_dates(self):
        #todo: allow this to focus on individual items
        self.tasks.verify_task_performance_times()
        children = self.taskList.get_children()
        items = [(x,self.taskList.item(x)) for x in children]
        for item_id,item in items:
            lastStoredDate,isCritical = self.tasks.getTaskPerformanceDate(item['text'])
            self.set_performance_column(item_id,lastStoredDate,isCritical)
            
            
    def set_performance_column(self,item,date,isCritical):
        if isCritical:
            date = "*** {} ***".format(date)
        self.taskList.set(item,column=TASK_DATE_COLUMN,value=date)

                    
    def destroy(self):
        """ Cleanup the window  """
        logging.debug("Destroying task window")
        #save the performance file:
        self.tasks.save_task_performance_times()
        
        #remove the reference in root:
        self.root.destroy_tasks()
        tk.Toplevel.destroy(self)
        
