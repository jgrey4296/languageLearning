import logging
from os import listdir
from os.path import isfile,join
from functools import partial
from collections import namedtuple

TITLE_DELIM = '*'
INGREDIENTS = 'ingredients'
INSTRUCTIONS = 'instructions'


Recipe = namedtuple('Recipe','name ingredients instructions originalFile marked')

class RecipeStore:

    def __init__(self,dataDir,recipe_const):
        logging.debug("Creating Recipe Store")
        self.data_directory = dataDir
        self.recipe_const = recipe_const
        #Dictionary of name -> Recipes
        self.recipes = {}
        self.loadRecipes()

    def getRecipeNames(self):
        return self.recipes.keys()

    def getRecipe(self,name):
        if not name in self.recipes:
            raise Exception("Recipe not found: {}".format(name))
        return self.recipes[name]

    def loadRecipes(self):
        logging.debug("Loading Recipes from: {} of type: {}".format(self.data_directory,self.recipe_const))
        try:
            #get all recipes:
            #TODO: Exclude all backup files
            recipes = [f for f in listdir(self.data_directory) if isfile(join(self.data_directory,f)) and \
                       self.recipe_const in f.lower()]
            logging.info("Recipes found: {}".format(recipes))
            #Process all recipes:
            for r in recipes:
                with open(join(self.data_directory,r)) as f:
                    try:
                        data = f.read()
                        name,ingredients,instructions = self.process_recipe(data.splitlines())
                        if name is not None:
                            logging.info("Loading recipe: {}".format(name))
                            tempName = name
                            i = 0
                            while tempName in self.recipes:
                                i += 1
                                tempName = "{}_{}".format(name,i)
                            markForUpdate = False
                            self.recipes[tempName] = Recipe(tempName, ingredients,instructions,r,markForUpdate)
                    except Exception as e:
                        logging.exception("Recipe Load Exception: {}".format(e))
        except Exception as e:
            logging.exception("Recipe Access Exception: {}".format(e))

    def process_recipe(self,lines):
        """ Take the text of a recipe, extract the name, ingredients, and instructions
            as separate strings
        """
        logging.debug("Processing Recipe")
        #The Data to extract:
        name = None
        ingredients = None
        instructions = None

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
            if INGREDIENTS in title.lower():
                ingredients = "\n".join(filteredLines[line+1:sectionEnd])
            elif INSTRUCTIONS in title.lower():
                instructions = "\n".join(filteredLines[line+1:sectionEnd])
            else:
                name = title
        #----
        if name and ingredients and instructions:
            return (name,ingredients,instructions)
        else:
            logging.warning("Recipe processing failure")
            return (None,None,None)


    def updateRecipe(self,name,ingredients,instructions):
        if not name in self.recipes:
            return
        logging.debug("Checking to Update Recipe: {}".format(name))
        relevantRecipe = self.recipes[name]
        if relevantRecipe.ingredients != ingredients or relevantRecipe.instructions != instructions:
            logging.debug("Check Success, updating")
            newRecipe = Recipe(name,ingredients,instructions,relevantRecipe.originalFile,True)
            self.recipes[name] = newRecipe

    def resaveRecipes(self):
        raise Exception("TODO: resaveRecipes")
