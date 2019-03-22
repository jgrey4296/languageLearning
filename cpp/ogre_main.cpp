#include <iostream>
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#include <OpenGL/glext.h>
#include <GLUT/glut.h>

#include <Ogre/OgreRoot.h>
#include <Ogre/OgreCamera.h>
#include <Ogre/OgreSceneManager.h>
#include <Ogre/OgreRenderWindow.h>
#include <Ogre/OgreEntity.h>
#include <Ogre/OgreViewport.h>
#include <Ogre/OgreConfigFile.h>

using namespace std;

int main(int argc, char* argv[]){
    cout << "Test Ogre Program" << endl;

    //Relative to where its executed from, not binary location
    Ogre::Root *mRoot = new Ogre::Root("configs/plugins.cfg","configs/config.cfg","logs/main.log");
    if(!(mRoot->restoreConfig() || mRoot->showConfigDialog())){
        delete mRoot;
        return -1;
    }

    // setup resources
    // Only add the minimally required resource locations to load up the Ogre head mesh
    Ogre::ResourceGroupManager::getSingleton().addResourceLocation("/Users/jgrey/OgreSDK/Media/materials/programs", "FileSystem", "General");
    Ogre::ResourceGroupManager::getSingleton().addResourceLocation("/Users/jgrey/OgreSDK/Media/materials/programs/GLSL", "FileSystem", "General");
    Ogre::ResourceGroupManager::getSingleton().addResourceLocation("/Users/jgrey/OgreSDK/Media/materials/scripts", "FileSystem", "General");
    Ogre::ResourceGroupManager::getSingleton().addResourceLocation("/Users/jgrey/OgreSDK/Media/materials/textures", "FileSystem", "General");
    Ogre::ResourceGroupManager::getSingleton().addResourceLocation("/Users/jgrey/OgreSDK/Media/models", "FileSystem", "General");
    Ogre::ResourceGroupManager::getSingleton().addResourceLocation("/Users/jgrey/github/Ogre_test/data","FileSystem","General");


    //Create the window
    Ogre::RenderWindow *mWindow = mRoot->initialise(true, "initial Render Window");
    Ogre::SceneManager *sceneManager = mRoot->createSceneManager(Ogre::ST_GENERIC);

    Ogre::Camera *camera = sceneManager->createCamera("PlayerCam");

    camera->setPosition(Ogre::Vector3(0,0,80));
    camera->lookAt(Ogre::Vector3(0,0,-300));
    camera->setNearClipDistance(5);

    Ogre::Viewport* vp = mWindow->addViewport(camera);
    vp->setBackgroundColour(Ogre::ColourValue(0,0,0,0));
    camera->setAspectRatio(Ogre::Real(vp->getActualWidth()) / Ogre::Real(vp->getActualHeight()));

    Ogre::ResourceGroupManager::getSingleton().initialiseAllResourceGroups();

    //Ogre::Entity* ogreHead = sceneManager->createEntity("Head","ogreHead.mesh");
    Ogre::Entity* ogreHead = sceneManager->createEntity("Head","myTest.obj");
    Ogre::SceneNode* headNode = sceneManager->getRootSceneNode()->createChildSceneNode();
    headNode->attachObject(ogreHead);

    sceneManager->setAmbientLight(Ogre::ColourValue(0.5,0.5,0.5));

    //Run the system
    bool continueRunning = true;
    while(continueRunning){
        mRoot->renderOneFrame();
        headNode->rotate(Ogre::Vector3(0,1,0),Ogre::Radian(0.005));
        if(mWindow->isClosed()){
            continueRunning = false;
        }

    }

    delete mRoot;
}
