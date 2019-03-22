# Simple Ogre Program
[ogre](ogre3d.org)'s build system has always been frustratingly complicated. 
This program demonstrates a simple, makefile based method for compiling an ogre program manually, primarily for *macOS*.
There is a single source file `main.cpp`, which will create a scene with the rotating ogre head.
This was built using *Ogre 1.9*

## Getting to work on other systems:
1) Create a `Frameworks` directory at the top level, beside the `makefile`, and put the Ogre frameworks in there, and update `configs/plugins.cfg` to have the full path to there  
2) Update the location of `${BOOST}` in the `makefile`  
3) Update the *resource locations* specified in `main.cpp`:29-33  
