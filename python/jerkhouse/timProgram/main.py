import os
from os.path import isdir
import tkinter as tk
import logging
import time
from gui.taskwindow import TaskWindow
from gui.recipewindow import RecipeWindow
from gui.contactwindow import ContactWindow
import subprocess


LOGLEVEL = logging.DEBUG

DATA_DIRECTORY = "data"      #where data is found
RECIPE_CONSTANT = "recipe"   #file base name. eg: aSandwichRecipe.txt
CONTACT_CONSTANT = "contact" #file base name. eg: bobNBillContact.txt
TASK_CONSTANT = "task"       #file base name. eg: orderwineTask.txt
MAIN_SIZE = 300

class Application(tk.Frame):
    """ Top level of Tim-GUI app. Creates the initial windowx  """

    def __init__(self,master=None):
        """ Create the top level access to each of sub-elements of the program  """
        super().__init__(master)
        #Enforce a size for the window:
        master.minsize(width=MAIN_SIZE,height=MAIN_SIZE)
        logging.debug("Creating Top level application")
        self.data_dir = DATA_DIRECTORY
        self.recipe_const = RECIPE_CONSTANT
        self.contact_const = CONTACT_CONSTANT
        self.task_const = TASK_CONSTANT
        self.pack()
        self.create_widgets()

        #Sub windows (kept track of for cleanup):
        self.taskWindow = None
        self.recipeWindow = None
        self.contactWindow = None

    def create_widgets(self):
        """ Instantiate the buttons that will open other windows  """
        #Make a button to open the tasks
        #TODO: change to grid layout
        logging.debug("Creating Widgets")
        self.taskButton = tk.Button(self)
        self.taskButton['text'] = "Tasks"
        self.taskButton['command'] = self.open_tasks
        self.taskButton.pack()

        #Make a button to open recipes
        self.recipesButton = tk.Button(self)
        self.recipesButton['text'] = "Recipes"
        self.recipesButton['command'] = self.open_recipes
        self.recipesButton.pack()

        #Make a button to open contact
        self.contactButton = tk.Button(self)
        self.contactButton['text'] = "Contact"
        self.contactButton['command'] = self.open_contacts
        self.contactButton.pack()

        #Open Data Directory button:
        self.dataButton = tk.Button(self)
        self.dataButton['text'] = "Data Directory"
        self.dataButton['command'] = self.open_data_directory
        self.dataButton.pack()

        #Create a Quit button
        self.quit = tk.Button(self,text="Quit", fg="red",command=root.destroy)
        self.quit.pack(side="bottom")

    def destroy(self):
        """ Customised destroy method  """
        logging.info("Closing program")
        tk.Frame.destroy(self)

    #----------
    def open_tasks(self):
        """ Create only 1 task window, lift it to be at the front either way  """
        if self.taskWindow is None:
            logging.debug("Creating Task window")
            self.taskWindow = TaskWindow(self)
        self.taskWindow.lift()

    def destroy_tasks(self):
        self.taskWindow = None

    #----------
    def open_recipes(self):
        """ Create only 1 recipe window, lift it to be at the front either way  """
        if self.recipeWindow is None:
            logging.debug("Creating Recipe window")
            self.recipeWindow = RecipeWindow(self)
        self.recipeWindow.lift()

    def destroy_recipes(self):
        self.recipeWindow = None

    #----------
    def open_contacts(self):
        if self.contactWindow is None:
            logging.debug("Creating Contact window")
            self.contactWindow = ContactWindow(self)
        self.contactWindow.lift()

    def destroy_contacts(self):
        self.contactWindow = None

    #----------
    def open_data_directory(self):
        subprocess.call(['open',DATA_DIRECTORY])



########################################
#                    MAIN
########################################
if __name__ == "__main__":
    #create the log directory if need be
    if not isdir('logs'):
        os.mkdir('./logs')
    if not isdir('./data'):
        os.mkdir('./data')
    #Setup logging:
    currentTime = time.gmtime()
    logfilename = "./logs/{}-{}-{}-{}.log".format(currentTime.tm_year,
                                                     currentTime.tm_mon,
                                                     currentTime.tm_mday,
                                                     currentTime.tm_hour)
    logging.basicConfig(filename=logfilename,level=LOGLEVEL,filemode='w')
    #console = logging.StreamHandler()
    #console.setLevel(logging.INFO)
    #logging.getLogger('').addHandler(console)
    #Start the program:
    root = tk.Tk()
    app = Application(master=root)
    app.mainloop()
