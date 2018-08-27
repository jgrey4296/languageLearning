import tkinter as tk
import tkinter.ttk as ttk
import logging
from os import listdir
from os.path import isfile,join
from functools import partial
import IPython

from components.recipeStore import RecipeStore,Recipe

TREEVIEW_ACTION = "<Button-1>"
EMPTY_LABEL_TEXT = "          \n          \n"
DEFAULT_LABEL_SIZE = 15

class RecipeWindow(tk.Toplevel):
    """ A Window for displaying recipes """

    def __init__(self,root):
        """ Initialise the window, creating a recipe store that loads recipes from data """
        logging.debug("Creating Recipe Window")
        tk.Toplevel.__init__(self)
        self.root = root
        self.recipes = RecipeStore(self.root.data_dir,self.root.recipe_const)
        self.create_widgets()
            
    def create_widgets(self):
        """ Create all the widgets in the window. See design notes for details """
        logging.debug("Creaing Recipe Widgets")
        #Recipe list on the left, as a tree
        self.recipeList = ttk.Treeview(self)
        self.recipeList.pack(side="left")
        #get all the recipes
        theRecipes = self.recipes.getRecipeNames()
        #add them to the treeview
        for i,recipeName in enumerate(sorted(theRecipes)):
            logging.info("Making button for: {}".format(recipeName))
            #CORE UPDATE ACTION:
            newEntry = self.recipeList.insert('',i)
            self.recipeList.item(newEntry,text=recipeName)
        #bind the update function to the treeview:
        self.recipeList.bind(TREEVIEW_ACTION,self.display_recipe_listener)
                        
        #Separate the Recipe List from the Recipe display:
        ttk.Separator(self,orient=tk.VERTICAL).pack(side="left",fill=tk.Y,expand=True)
        
        #Recipe Frame:
        self.recipeFrame = tk.Frame(self)
        self.recipeFrame.pack(side="right")

        #Recipe Title
        self.recipeLabel = tk.Label(self.recipeFrame,borderwidth=5)
        self.recipeLabel.grid(row=0,column=1)

        ttk.Separator(self.recipeFrame,orient=tk.HORIZONTAL).grid(row=1,column=0,columnspan=3,sticky=tk.E+tk.W)
        ingredientLabel = tk.Label(self.recipeFrame,borderwidth=5)
        ingredientLabel['text'] = "INGREDIENTS"
        ingredientLabel.grid(row=2,column=0)

        instructionLabel = tk.Label(self.recipeFrame,borderwidth=5)
        instructionLabel['text'] = "INSTRUCTIONS"
        instructionLabel.grid(row=2,column=2)
        
        #separate title from the rest
        ttk.Separator(self.recipeFrame,orient=tk.HORIZONTAL).grid(row=3,column=0,columnspan=3,sticky=tk.E+tk.W)
        #Separate the Ingredients (column 0) from the instructions (column 2)
        ttk.Separator(self.recipeFrame,orient=tk.VERTICAL).grid(row=3,column=1,rowspan=4,sticky=tk.N+tk.S)

        #CORE DISPLAYS:
        #recipe ingredients
        self.recipeIngredients = tk.Label(self.recipeFrame,width=DEFAULT_LABEL_SIZE,height=DEFAULT_LABEL_SIZE,padx=DEFAULT_LABEL_SIZE,pady=DEFAULT_LABEL_SIZE)
        self.recipeIngredients.grid(row=4,column=0)
        self.recipeIngredients['text'] = EMPTY_LABEL_TEXT
        
        #recipe instructions
        self.recipeInstructions = tk.Label(self.recipeFrame,width=DEFAULT_LABEL_SIZE,height=DEFAULT_LABEL_SIZE,padx=DEFAULT_LABEL_SIZE,pady=DEFAULT_LABEL_SIZE)
        self.recipeInstructions.grid(row=4,column=2)
        self.recipeIngredients['text'] = EMPTY_LABEL_TEXT


    def display_recipe_listener(self,event):
        """ The Update method called when a recipe button is pressed """
        item = self.recipeList.identify('item',event.x,event.y)
        recipeName = self.recipeList.item(item,'text')
        recipe = self.recipes.getRecipe(recipeName)
        
        logging.debug("Displaying Recipe: {}".format(recipeName))
        if not isinstance(recipe,Recipe):
            raise Exception("Trying to display something that isnt a Recipe")
        
        self.recipeLabel['text'] = recipeName
        self.recipeIngredients['text'] = recipe.ingredients
        self.recipeInstructions['text'] = recipe.instructions
        
    def destroy(self):
        """ Cleanup the window  """
        logging.debug("Destroying recipe window")
        #remove the reference in root:
        self.root.destroy_recipes()
        tk.Toplevel.destroy(self)
        
