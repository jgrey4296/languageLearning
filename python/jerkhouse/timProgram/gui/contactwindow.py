import tkinter as tk
import tkinter.ttk as ttk
import logging
from os import listdir
from os.path import isfile,join
from functools import partial
import IPython

from components.contactStore import ContactStore,Contact

TREEVIEW_ACTION = "<Button-1>"
EMPTY_LABEL_TEXT = "          \n          \n"
DEFAULT_LABEL_SIZE = 15

class ContactWindow(tk.Toplevel):
    """ A Window for displaying contacts """

    def __init__(self,root):
        """ Initialise the window, creating a contact store that loads contacts from data """
        logging.debug("Creating Contact Window")
        tk.Toplevel.__init__(self)
        self.root = root
        self.contacts = ContactStore(self.root.data_dir,self.root.contact_const)
        self.create_widgets()
            
    def create_widgets(self):
        """ Create all the widgets in the window. See design notes for details """
        logging.debug("Creaing Contact Widgets")
        #Contact list on the left, as a tree
        self.contactList = ttk.Treeview(self)
        self.contactList.pack(side="left")
        #get all the contacts
        theContacts = self.contacts.getContactNames()
        #add them to the treeview
        for i,contactName in enumerate(sorted(theContacts)):
            logging.info("Making button for: {}".format(contactName))
            #CORE UPDATE ACTION:
            newEntry = self.contactList.insert('',i)
            self.contactList.item(newEntry,text=contactName)
        #bind the update function to the treeview:
        self.contactList.bind(TREEVIEW_ACTION,self.display_contact_listener)
                        
        #Separate the Contact List from the Contact display:
        ttk.Separator(self,orient=tk.VERTICAL).pack(side="left",fill=tk.Y,expand=True)
        
        #Contact Frame:
        self.contactFrame = tk.Frame(self)
        self.contactFrame.pack(side="right")

        #Contact Title
        self.contactLabel = tk.Label(self.contactFrame,borderwidth=5)
        self.contactLabel.grid(row=0,column=1)

        #separate title from the rest
        ttk.Separator(self.contactFrame,orient=tk.HORIZONTAL).grid(row=1,column=0,columnspan=3,sticky=tk.E+tk.W)

        #CORE DISPLAYS: address, phone, email
        self.contactAddress = tk.Label(self.contactFrame,width=DEFAULT_LABEL_SIZE,height=DEFAULT_LABEL_SIZE,padx=DEFAULT_LABEL_SIZE,pady=DEFAULT_LABEL_SIZE)
        self.contactPhone = tk.Label(self.contactFrame,width=DEFAULT_LABEL_SIZE,height=DEFAULT_LABEL_SIZE,padx=DEFAULT_LABEL_SIZE,pady=DEFAULT_LABEL_SIZE)
        self.contactEmail = tk.Label(self.contactFrame,width=DEFAULT_LABEL_SIZE,height=DEFAULT_LABEL_SIZE,padx=DEFAULT_LABEL_SIZE,pady=DEFAULT_LABEL_SIZE)

        addressLabel = tk.Label(self.contactFrame)
        addressLabel['text'] = "ADDRESS"
        addressLabel.grid(row=2,column=1)
        ttk.Separator(self.contactFrame,orient=tk.HORIZONTAL).grid(row=3,column=0,columnspan=3,sticky=tk.E+tk.W)
        
        self.contactAddress.grid(row=4,column=1)
        self.contactAddress['text'] = EMPTY_LABEL_TEXT

        ttk.Separator(self.contactFrame,orient=tk.HORIZONTAL).grid(row=5,column=0,columnspan=3,sticky=tk.E+tk.W)
        phoneLabel = tk.Label(self.contactFrame)
        phoneLabel['text'] = "PHONE NUMBER"
        phoneLabel.grid(row=6,column=1)
        ttk.Separator(self.contactFrame,orient=tk.HORIZONTAL).grid(row=7,column=0,columnspan=3,sticky=tk.E+tk.W)
        
        self.contactPhone.grid(row=8,column=1)
        self.contactPhone['text'] = EMPTY_LABEL_TEXT

        ttk.Separator(self.contactFrame,orient=tk.HORIZONTAL).grid(row=9,column=0,columnspan=3,sticky=tk.E+tk.W)
        emailLabel = tk.Label(self.contactFrame)
        emailLabel['text'] = "EMAIL ADDRESS"
        emailLabel.grid(row=10,column=1)
        ttk.Separator(self.contactFrame,orient=tk.HORIZONTAL).grid(row=11,column=0,columnspan=3,sticky=tk.E+tk.W)
                
        self.contactEmail.grid(row=12,column=1)
        self.contactPhone['text'] = EMPTY_LABEL_TEXT


    def display_contact_listener(self,event):
        """ The Update method called when a contact button is pressed """
        item = self.contactList.identify('item',event.x,event.y)
        contactName = self.contactList.item(item,'text')
        contact = self.contacts.getContact(contactName)
        
        logging.debug("Displaying Contact: {}".format(contactName))
        if not isinstance(contact,Contact):
            raise Exception("Trying to display something that isnt a Contact")
        
        self.contactLabel['text'] = contactName
        self.contactAddress['text'] = contact.address
        self.contactPhone['text'] = contact.phone
        self.contactEmail['text'] = contact.email
        
    def destroy(self):
        """ Cleanup the window  """
        logging.debug("Destroying contact window")
        #remove the reference in root:
        self.root.destroy_contacts()
        tk.Toplevel.destroy(self)
        
