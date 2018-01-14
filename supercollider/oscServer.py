"""Small example OSC server

This program listens to several addresses, and prints some information about
received packets.
"""
import threading
from pythonosc import dispatcher
from pythonosc import osc_server
from pythonosc import osc_message_builder
from pythonosc import udp_client


if __name__ == "__main__":
  dispatcher = dispatcher.Dispatcher()
  dispatcher.map("/hello", print)
  
  server = osc_server.ForkingOSCUDPServer(("127.0.0.1", 7771), dispatcher)
  server_thread = threading.Thread(target=server.serve_forever)
  server_thread.start()

  #Send a message
  client = udp_client.SimpleUDPClient("127.0.0.1", 7772)
  for x in range(10):
      client.send_message("/test", "blah")
  
  
  # server = osc_server.ThreadingOSCUDPServer(
  #     ("127.0.0.1", 7771), dispatcher)
  # print("Serving on {}".format(server.server_address))
  #server.serve_forever()

  
