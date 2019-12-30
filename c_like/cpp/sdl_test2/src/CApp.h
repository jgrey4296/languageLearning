#pragma once

#include <stdint.h>
#include "SDL2/SDL.h"
#include <boost/lambda/lambda.hpp>
#include <stdexcept>
#include <iostream>
#include <iterator>
#include <algorithm>
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#include <OpenGL/glext.h>
#include <GLUT/glut.h>
#include <lua.hpp>

const int MAXRUNCOUNT = 10;
const GLubyte red[] = { 255, 0, 0,255};
const GLubyte other[] = { 0, 255, 0, 255};

class CApp {
    int runCount;
    bool Running;
    SDL_Window* win;
    SDL_GLContext glctx;
    
 public:
    CApp();
    void OnInit();
    int OnExecute();
    void OnEvent (SDL_Event* Event);
    void OnLoop();
    void OnRender();
    void OnCleanup();
};
