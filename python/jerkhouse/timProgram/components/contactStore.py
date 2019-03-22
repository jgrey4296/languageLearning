import logging
from os import listdir
from os.path import isfile,join
from functools import partial
from collections import namedtuple

TITLE_DELIM = '*'
ADDRESS = "address"
PHONE = "phone"
EMAIL = "email"

Contact = namedtuple('Contact','name address phone email originalFile marked')

class ContactStore:

    def __init__(self,dataDir,contact_const):
        logging.debug("Creating Contact Store")
        self.data_directory = dataDir
        self.contact_const = contact_const
        #Dictionary of name -> Contacts
        self.contacts = {}
        self.loadContacts()

    def getContactNames(self):
        return self.contacts.keys()

    def getContact(self,name):
        if not name in self.contacts:
            raise Exception("Contact not found: {}".format(name))
        return self.contacts[name]

    def loadContacts(self):
        logging.debug("Loading Contacts from: {} of type: {}".format(self.data_directory,self.contact_const))
        try:
            #get all contacts:
            #TODO: Exclude all backup files
            contacts = [f for f in listdir(self.data_directory) if isfile(join(self.data_directory,f)) and \
                       self.contact_const in f.lower()]
            logging.info("Contacts found: {}".format(contacts))
            #Process all contacts:
            for c in contacts:
                with open(join(self.data_directory,c)) as f:
                    try:
                        data = f.read()
                        name,address,phone,email = self.process_contact(data.splitlines())
                        if name is not None:
                            logging.info("Loading contact: {}".format(name))
                            tempName = name
                            i = 0
                            while tempName in self.contacts:
                                i += 1
                                tempName = "{}_{}".format(name,i)
                            markForUpdate = False
                            self.contacts[tempName] = Contact(tempName,address,phone,email,c,markForUpdate)
                    except Exception as e:
                        logging.exception("Contact Load Exception: {}".format(e))
        except Exception as e:
            logging.exception("Contact Access Exception: {}".format(e))

    def process_contact(self,lines):
        """ Take the text of a contact, extract the name,address,phone,email
        """
        logging.debug("Processing Contact")
        #The Data to extract:
        name = None
        address = None
        phone = None
        email = None

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
            if ADDRESS in title.lower():
                address = "\n".join(filteredLines[line+1:sectionEnd])
            elif PHONE in title.lower():
                phone = "\n".join(filteredLines[line+1:sectionEnd])
            elif EMAIL in title.lower():
                email = "\n".join(filteredLines[line+1:sectionEnd])
            else:
                name = title
        #----
        if name and (address or phone or email or instructions):
            return (name,address,phone,email)
        else:
            logging.warning("Contact processing failure")
            return (None,None,None,None)

    def resaveContacts(self):
        raise Exception("TODO: resaveContacts")
