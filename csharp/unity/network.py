import logging as root_logger
import socket
import json
from enum import Enum
from json.decoder import JSONDecodeError
import IPython

import logging as root_logger
logging = root_logger.getLogger(__name__)
####################
DEFAULT_PORT = 50000
DEFAULT_BLOCKSIZE = 1024
DEFAULT_BACKLOG = 5
DEFAULT_HOST = "localhost"


class UnityServer:
    """ A Server to connect to unity """
    
    #The types of messages supported
    MESSAGE_T = Enum("Message Types", "S_HANDSHAKE C_HANDSHAKE INFO ACTION AI_GO AI_COMPLETE QUIT", start=0)

    def __init__(self,
                 port=DEFAULT_PORT,
                 host=DEFAULT_HOST,
                 backlog=DEFAULT_BACKLOG,
                 blockSize=DEFAULT_BLOCKSIZE):
        self.theSocket  = None
        self.host = host
        self.port = port
        self.backlog = backlog 
        self.blockSize = blockSize

        self.fromClientMessages = []
        self.toClientMessages = []
        self.data_segments = []
        self.data_size = 0

        self.accepted_client = None
        self.listen_for_clients = True
        self.listen_for_data = True
    
    #Methods:
    def setup(self):
        logging.info("Setting up Python Server")
        self.theSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.theSocket.bind((self.host, self.port)) 
        self.theSocket.listen(self.backlog)

    def close(self):
        logging.info("Closing Socket")
        if self.accepted_client is not None:
            self.accepted_client.close()
        if self.theSocket is not None:
            self.theSocket.close()
        self.listen_for_clients = False
        self.listen_for_data = False

        
    def listen(self):
        logging.info("Listening")
        if self.theSocket is None:
            raise Exception("Socket is not setup")
        try:
            while self.listen_for_clients:
                #Loop for a connection
                client, address = self.theSocket.accept()
                self.accepted_client = client
                self.listen_for_data = True
                logging.info("Client connected on: {}.".format(str(address)))
                while self.listen_for_data:
                    #loop to actually recieve data
                    logging.info("--------------------")
                    logging.info('Waiting for data')
                    data = self.accepted_client.recv(self.blockSize).decode()
                    if not bool(data):
                        logging.info("Nothing Received")
                        IPython.embed(simple_prompt=True)
                        continue
                    logging.info('Data: {}'.format(str(data)))
                    try:
                        self.consume_data_segment(data)
                    except JSONDecodeError:
                        logging.info("Problem Decoding")
                    except TypeError as e:
                        logging.warning("Problem Encoding")
                        logging.warning("{}".format(str(e)))
                    #Data has been recieved and added, now do something with it
                    if not bool(self.data_size):
                        self.consume_data()
                        
            #Finished looping with this client:
            logging.info("Closing Client")
            client.close()
        finally:
            #finished listening entirely
            logging.info("Finishing Listening")
            self.theSocket.close()
            self.theSocket = None

    def consume_data_segment(self, data):
        """ consume and add a segment of data """
        self.data_segments.append(data)
            
    def consume_data(self):
        """ Consume and handle an entire message header """
        data = "".join(self.data_segments)
        self.data_segments.clear()
        if not bool(data):
            return
        try:
            decoded = json.loads(data.strip());
        except json.decoder.JSONDecodeError:
            IPython.embed(simple_prompt=True)
        response = self.handleJson(decoded)
        logging.info("Continue: {}, Response: {}".format(self.listen_for_data, response))
        if response is not None:
            self.respond(response)

            
    def handleJson(self, data):
        logging.info("Handling Data: {}".format(str(data)))
        response = None
        if not all([x in data for x in ["size", "iden", "data"]]):
            logging.warning("Data is malformed")
            self.listen_for_data = False
        elif data['iden'] == UnityServer.MESSAGE_T.C_HANDSHAKE.value:
            logging.info('Startup')
            response = data.copy()
            response['iden'] = UnityServer.MESSAGE_T.S_HANDSHAKE.value
        elif data['iden'] == UnityServer.MESSAGE_T.QUIT.value:
            logging.info('Quitting')
            self.close()
        else:
            logging.info("Other: {}".format(data['iden']))
            self.listen_for_data = False

        return response

    def respond(self, data):
        logging.info("Responding: {}".format(data))
        if data is None:
            raise Exception("Response Data shouldn't be None")
        
        if not isinstance(data, str):
            data = json.dumps(data)
            #todo: change this to use the same byte array each time, without creating a new object
            byteData = bytearray(data + "\n", 'utf-8')
            self.accepted_client.sendall(byteData)


            
if __name__ == "__main__":
    # Setup root_logger:
    import logging as root_logger
    LOGLEVEL = root_logger.DEBUG
    LOG_FILE_NAME = "log.Network"
    root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')
    
    console = root_logger.StreamHandler()
    console.setLevel(root_logger.INFO)
    root_logger.getLogger('').addHandler(console)
    logging = root_logger.getLogger(__name__)
    ##############################
    logging.info("Setting up Server")
    sev = UnityServer()
    try:
        sev.setup()
        sev.listen()
    except (KeyboardInterrupt):
        logging.info("Shutting down")
        sev.close()

    
