// $Id: hello-gl.cpp,v 1.20 2016-01-19 17:48:55-08 - - $

// Display "Hello World" in a window.

#include <iostream>
#include <string>
using namespace std;

//OS dependent imports:
#ifdef __APPLE__
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#include <GLUT/glut.h>
#else
#include <GL/gl.h>
#include <GL/freeglut.h>
#include <GL/glu.h>
#endif

#include <libgen.h>

struct {
   string name;
   int width {256};
   int height {192};
   string message {"Hello, World!"};
   const GLubyte* msg_text() {
       return reinterpret_cast<const GLubyte*> (message.c_str());
   }
} window;

void display() {
      //Standard initialisation stuff:
    //Clear the buffers being used
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); 
    glEnable(GL_DEPTH_TEST);
    //Set the matrix mode to deal with your 'camera'
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity(); 
    
    //Viewpoint:
    //Left, right, bottom, top, near, far    
    glOrtho(-0.0, 4.0, -0.0, 4.0, -2.0, 500.0); 
    
    //switch back to dealing with 'what' you are drawing
    glMatrixMode(GL_MODELVIEW);
    
    glColor3ub(0x00,0x00,0xFF);
    //draw a triangle:
    glBegin(GL_POLYGON);
    glVertex2f(0,0);
    glVertex2f(0,1);
    glVertex2f(1,0);
    glEnd();


    //Draw some text
   void* font = GLUT_BITMAP_TIMES_ROMAN_24;
   int width = glutBitmapLength (font, window.msg_text());
   int height = 25;
   glColor3ub (0xFF, 0xFF, 0xFF);
   //GLfloat xpos = window.width / 2.0 - width / 2.0;
   //GLfloat ypos = window.height / 2.0 - height / 4.0;
   glRasterPos2f (1, 2);
   for(auto i = window.message.begin(); i != window.message.end(); i++){
       char c = *i;
       glutBitmapCharacter(font,c);
   }
   glutSwapBuffers();
}

void reshape (int width, int height) {
   cout << __func__ << "(" << width << "," << height << ")" << endl;
   window.width = width;
   window.height = height; 
   glMatrixMode (GL_PROJECTION);
   glLoadIdentity();
   gluOrtho2D (0, window.width, 0, window.height);
   glMatrixMode (GL_MODELVIEW);
   glViewport (0, 0, window.width, window.height);
   glutPostRedisplay();
}

int main (int argc, char** argv) {
   window.name = basename (argv[0]);
   glutInit (&argc, argv);
   glutInitDisplayMode (GLUT_RGBA | GLUT_DOUBLE);
   glutInitWindowSize (window.width, window.height);
   glutCreateWindow (window.name.c_str());
   glutDisplayFunc (display);
   glutReshapeFunc (reshape);
   glutMainLoop();
   return 0;
}

