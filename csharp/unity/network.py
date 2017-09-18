import logging as root_logger
import socket
import json
from json.decoder import JSONDecodeError
import IPython

# Setup root_logger:
LOGLEVEL = root_logger.DEBUG
LOG_FILE_NAME = "log.network"
root_logger.basicConfig(filename=LOG_FILE_NAME, level=LOGLEVEL, filemode='w')
console = root_logger.StreamHandler()
console.setLevel(root_logger.INFO)
root_logger.getLogger('').addHandler(console)
logging = root_logger.getLogger(__name__)

#NOTE: TO CLOSE A SOCKET, in BASH:
#Constants:
host = 'localhost' 
port = 50000
backlog = 5 
blockSize = 1024
theSocket  = None

#Functions:
def setup():
    logging.info("Setting up")
    global theSocket
    theSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    theSocket.bind((host,port)) 
    theSocket.listen(backlog)

def listen():
    logging.info("Listening")
    global theSocket
    if theSocket is None:
        logging.exception("The Socket is Not Set Up")
    try:
        listen_loop = True
        while listen_loop:
            client, address = theSocket.accept()
            client_loop = True
            logging.info("Client connected on: {}.".format(str(address)))
            while client_loop:
                logging.info("--------------------")
                logging.info('Waiting for data')
                data = client.recv(blockSize).decode()
                if data == "":
                    logging.info("Nothing Received")
                    continue
                logging.info('Data: {}'.format(str(data)))
                try:
                    decoded = json.loads(data);
                    client_loop, response = handleJson(decoded)
                    logging.info("Continue: {}, Response: {}".format(client_loop, response))
                    if response is not None:
                        respond(client, response)
                except JSONDecodeError:
                    logging.info("Problem Decoding")
                except TypeError as e:
                    logging.warning("Problem Encoding")
                    logging.warning("{}".format(str(e)))
            #Finished looping with this client:
            logging.info("Closing Client")
            client.close()
        
    finally:
        #finished listening entirely
        logging.info("Finishing Listening")
        theSocket.close()
        theSocket = None
    
def handleJson(data):
    logging.info("Handling Data: {}".format(str(data)))
    cont = True
    response = None
    if not 'name' in data:
        logging.warning("Data lacks a name")
        cont = False
        return (cont, response)
        
    if data['name'] == "Handshake":
        logging.info('Startup')
        response = "blah"
    elif data['name'] == "ping":
        response = "blah"
    elif data['name'] == "end":
        cont = False
    else:
        logging.info("Closing")
        cont = False
    return (cont, response)

def respond(client, data):
    logging.info("Responding")
    if data is None:
        return
    if not isinstance(data, str):
        data = json.dumps(data)
    #todo: change this to use the same byte array each time, without creating a new object
    byteData = bytearray(data + "\n", 'utf-8')
    client.sendall(byteData)

if __name__ == "__main__":
    try:
        setup()
        listen()
    except (KeyboardInterrupt):
        logging.info("Shutting down")
        if theSocket is not None:
            theSocket.close()

    
