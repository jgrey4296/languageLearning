"""
This is a setup.py script generated by py2applet

Usage:
    python setup.py py2app
"""

from setuptools import setup
import sys
sys.setrecursionlimit(1500)

#Note the addition of py locations:
APP = ['timProgram/main.py']
DATA_FILES = []
OPTIONS = {'argv_emulation': True,
           'plist' : {
           'PyRuntimeLocations': [
               '@executable_path/../Frameworks/libpython3.5m.dylib',
           ]},
           'excludes' : [
               'PIL','cairo','numpy','scipy','cryptography','sip','matplotlib','docutils',
               'zmq'
           ]}

setup(
    app=APP,
    data_files=DATA_FILES,
    options={'py2app': OPTIONS},
    setup_requires=['py2app'],
)
