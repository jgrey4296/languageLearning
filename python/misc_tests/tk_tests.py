import tkinter as tk
import logging
import tkinter.ttk as ttk
import IPython

root = tk.Tk()

label = tk.Label(root)
label['text'] = "blahhhh"
label.grid()

tree = ttk.Treeview(root)
tree.grid()

def printSelection(event):
    item = tree.identify('item',event.x,event.y)
    print("clicked on {}".format(tree.item(item,'text')))

tree.bind("<Button-1>", printSelection)


a = tree.insert('',0)
b = tree.insert('',1)
aa = tree.insert(a,0)

tree.item(a,text='abl')
tree.item(b,text='aweg')
tree.item(aa,text='awegg')


root.mainloop()

#partial application reminder:
#action = partial(lambda x: self.display_recipe(x,self.recipes.getRecipe(x)),recipeName)
#recipeButton = tk.Button(self.recipeList,text=recipeName,command=action)
#recipeButton.grid(row=i)
