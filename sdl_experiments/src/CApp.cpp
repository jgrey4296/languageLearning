#include "CApp.h"
#include <string.h>

using namespace std;

/**
   The CApp Ctor. 
 */
CApp::CApp() : runCount(0), Running(true){}

//ARRAY BUFFER
const float vertexPositions[] = {
    0.75f, 0.75f, 0.0f, 1.0f,
    0.75f, -0.75f, 0.0f, 1.0f,
    -0.75f, -0.75f, 0.0f, 1.0f,
};
GLuint positionBufferObject;
//VERTEX SHADER
string strVertexShader = "#version 120\n void main() {\
 vec4 a = gl_Vertex; \
 a.x = a.x * 0.2;\
a.y = a.y + (a[0]); \
 gl_Position = gl_ModelViewProjectionMatrix * a; }";

const GLchar* vSrc = (const GLchar *) strVertexShader.c_str();
//FRAGMENT SHADER
string strFragmentShader = "#version 120\n void main () { gl_FragColor = vec4(0.2, 0.4, 0.5, 0.3); }";
const GLchar* fSrc = (const GLchar*) strFragmentShader.c_str();
GLuint shader;
GLint result = GL_FALSE;
int info;


void Initialise(){
    cout << "init vertex buffer and shader" << endl;
    cout << glGetString(GL_VERSION) << endl;
    //cout << glGetString(GL_EXTENSIONS) << endl;
    //setup the vertex array buffer
    glGenBuffers(1,&positionBufferObject);
    glBindBuffer(GL_ARRAY_BUFFER, positionBufferObject);
    glBufferData(GL_ARRAY_BUFFER,sizeof(vertexPositions),vertexPositions,GL_STATIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER,0);

    cout << "setup the vertex shader" <<endl;
    GLuint vertexShader = glCreateShader(GL_VERTEX_SHADER);
    cout << "setting shader source" << endl;
    glShaderSource(vertexShader, 1, &vSrc, 0);
    cout << "compiling" << endl;
    glCompileShader(vertexShader);

    //check
    glGetShaderiv(vertexShader,GL_COMPILE_STATUS,&result);
    glGetShaderiv(vertexShader,GL_INFO_LOG_LENGTH,&info);
    if(info > 0){
        cout << "error " << endl;
        vector<char> message(info-1);
        glGetShaderInfoLog(vertexShader,info,NULL,&message[0]);
        printf("%s\n",&message[0]);
    }
        
    cout << "setup the fragment shader" << endl;
    GLuint fragmentShader = glCreateShader(GL_FRAGMENT_SHADER);
    glShaderSource(fragmentShader,1, &fSrc,0);
    glCompileShader(fragmentShader);

    //check
    glGetShaderiv(fragmentShader,GL_COMPILE_STATUS,&result);
    glGetShaderiv(fragmentShader,GL_INFO_LOG_LENGTH,&info);
    if(info > 0){
        cout << "error " << endl;
        vector<char> message(info-1);
        glGetShaderInfoLog(fragmentShader,info,NULL,&message[0]);
        printf("%s\n",&message[0]);
    }
    
    cout << "link" << endl;
    GLuint programObject = glCreateProgram();
    glAttachShader(programObject,vertexShader);
    glAttachShader(programObject,fragmentShader);
    glLinkProgram(programObject);

    glDetachShader(programObject,vertexShader);
    glDetachShader(programObject,fragmentShader);

    //check:
    glGetProgramiv(programObject,GL_LINK_STATUS,&result);
    glGetProgramiv(programObject,GL_INFO_LOG_LENGTH, &info);
    if(info > 0){
        cout << "error " << endl;
        vector<char> message(info-1);
        glGetProgramInfoLog(programObject,info,NULL,&message[0]);
        printf("%s\n",&message[0]);
    }


    
    shader = programObject;
    cout << "shader" << shader << " program " << programObject << endl;
    cout << "finished initialising" << endl;
}


void CApp::OnInit(){
    //initialise SDL
    if(SDL_Init(SDL_INIT_EVERYTHING) != 0){
        throw runtime_error("temp");
    }
    //Create a window
    win = SDL_CreateWindow("Hello world", 100,100,640,480,
                           SDL_WINDOW_SHOWN | SDL_WINDOW_OPENGL | SDL_WINDOW_RESIZABLE);
    if(win == nullptr){ throw runtime_error("temp"); }
    
    //Create the opengl context
    glctx = SDL_GL_CreateContext(win);
    Initialise();
}

int CApp::OnExecute(){
    try{
        OnInit();
    }catch(runtime_error& e){
        cout << "An error occurred: " << SDL_GetError() << endl;
        OnCleanup();
        return -1;
    }
    
    SDL_Event Event;
    while(Running && runCount++ < MAXRUNCOUNT){
        while(SDL_PollEvent(&Event)){
            OnEvent(&Event);
        }
        OnLoop();
        OnRender();
    }

    OnCleanup();
    return 0;
}

void CApp::OnEvent(SDL_Event* Event){

}

void CApp::OnLoop(){

}

void CApp::OnRender(){
    glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glMatrixMode( GL_MODELVIEW );
    glLoadIdentity();

    cout << "using shader " << shader << endl;
    glUseProgram(shader);

    glBegin(GL_TRIANGLES);
    //glColor4ubv(red);
    glVertex3f(0,0,0);
    glVertex3f(0,25,0);
    //glColor4ubv(other);
    glVertex3f(25,25,0);    
    glEnd();


    //glutSolidSphere(1.0,32,32);
    //glutSolidTeapot(0.2f);
    

    // glBindBuffer(GL_ARRAY_BUFFER,positionBufferObject);
    // glEnableVertexAttribArray(0);
    // glVertexAttribPointer(0,4,GL_FLOAT, GL_FALSE, 0, 0);
    // glDrawArrays(GL_TRIANGLES, 0, 3);

    glUseProgram(0);
    SDL_GL_SwapWindow(win);
    SDL_Delay(1000);
}

void CApp::OnCleanup(){
    //TODO: more intelligent cleanup. Look at variadic templates

    SDL_GL_DeleteContext(glctx);
    if(win != nullptr) { SDL_DestroyWindow(win); }
    SDL_Quit();    
}
