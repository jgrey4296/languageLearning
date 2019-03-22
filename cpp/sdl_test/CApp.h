#ifndef __CAPP_H__
#define __CAPP_H__

#include "SDL2/SDL.h"
#include <boost/lambda/lambda.hpp>
#include <iostream>
#include <iterator>
#include <algorithm>
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#include <OpenGL/glext.h>
#include <GLUT/glut.h>

class CApp {
 public:
    CApp();
    int OnExecute();
};
#endif
