import IPython
import socket
import json
print('Starting up Python Server')

host = 'localhost' 
port = 50000
backlog = 5 
size = 1024 
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.bind((host,port)) 
s.listen(backlog) 
while 1:
    client, address = s.accept()
    print("Client connected.")
    while 1:
        print('Waiting for data')
        data = client.recv(size).decode()
        print('Data: {}'.format(str(data)))
        if data == "Handshake":
            print('Startup')
            client.sendall(b'pong\n')
        elif data == "ping":
            print ("Unity Sent: " + data)
            client.sendall(b'pong\n')
            
        else:
            client.sendall(b'other\n')
            print ("Unity Sent Something Else: " + str(data))
            client.close()
            break
