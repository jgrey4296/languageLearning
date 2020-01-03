#include "CApp.h"

using namespace std;

CApp::CApp(){

}

int CApp::OnExecute(){
    return 0;
}

int main(int argc, char* argv[]){
    cout << "Initial SDL Program" << endl;
    
    CApp theApp;
    return theApp.OnExecute();
}
