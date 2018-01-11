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
DEFAULT_HEADERSIZE = 128
DEFAULT_BACKLOG = 5
DEFAULT_HOST = "localhost"


class UnityServer:
    """ A Server to connect to unity """
    
    #The types of messages supported
    MESSAGE_T = Enum("Message Types", "HANDSHAKE INFO ACTION AI_GO AI_COMPLETE QUIT PAYLOAD", start=0)
    MESSAGE_T_LOOKUP = {x.value : x for x in MESSAGE_T}
    
    def __init__(self,
                 port=DEFAULT_PORT,
                 host=DEFAULT_HOST,
                 backlog=DEFAULT_BACKLOG,
                 blockSize=DEFAULT_BLOCKSIZE,
                 headerSize=DEFAULT_HEADERSIZE):
        self.theSocket  = None
        self.host = host
        self.port = port
        self.backlog = backlog
        #Static size of all headers, padded with '!'s at end
        self.headerSize = headerSize
        #the amount of data to read at a time for payloads
        self.blockSize = blockSize

        #queues of messages to process
        self.fromClientMessages = []
        self.toClientMessages = []
        #Unfinished segments of data
        self.data_segments = ""

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

        
    def listen_for_headers(self):
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
                    #RECEIVE HEADER:
                    header = ""
                    if bool(self.data_segments):
                        header += self.data_segments[:self.headerSize]
                        self.data_segments = self.data_segments[self.headerSize:]
                    while len(header) < self.headerSize:
                        #loop until enough data for a header has been received
                        header += self.accepted_client.recv(self.headerSize - len(header)).decode()
                    logging.info('Header: {}'.format(str(header)))
                    #Consume the header, either do something,
                    #or consume the payload afterwards
                    self.consume_header(header)
                        
            #Finished looping with this client:
            logging.info("Closing Client")
            client.close()
        finally:
            #finished listening entirely
            logging.info("Finishing Listening")
            self.close()

    def consume_header(self, data):
        """ Consume a header, in prep for more data possibly """
        assert(len(data) == self.headerSize);
        trimmed = data.strip("! ")
        decoded = json.loads(trimmed);
        assert('size' in decoded)
        assert('iden' in decoded)
        logging.info("Received a header, Type: {}".format(UnityServer.MESSAGE_T_LOOKUP[decoded['iden']]))

        #if a header only: act
        if (decoded['iden'] != UnityServer.MESSAGE_T.INFO.value):
            self.respond_to_header(decoded)
        else:
            self.respond_to_info(decoded)
        

    def respond_to_header(self, data):
        """ Given a Header that needs no payload, act upon it """
        if data['iden'] == UnityServer.MESSAGE_T.HANDSHAKE.value:
            self.respond({"size": 0, "iden": UnityServer.MESSAGE_T.HANDSHAKE.value, "data": ""})
        elif data['iden'] == UnityServer.MESSAGE_T.QUIT.value:
            self.close()
        
    def respond_to_info(self, data):
        """ Given an info header, listen for the payload """
        logging.info("Respond to Info not implemented yet")
        amount_to_listen_for = data['size']
        assert(amount_to_listen_for > 0)
        received = self.data_segments[:amount_to_listen_for]
        while len(received) < amount_to_listen_for:
            listenAmount = min(self.blockSize, amount_to_listen_for - len(received))
            received += self.accepted_client.recv(listenAmount).decode()
        data = received[:amount_to_listen_for]
        self.data_segments[amount_to_listen_for:]

        logging.info("Payload was: {}".format(data))

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
        sev.listen_for_headers()
    except (KeyboardInterrupt):
        logging.info("Shutting down")
        sev.close()

    
