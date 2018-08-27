#include "CApp.h"

using namespace std;



int main(int argc, char* argv[]){
    cout << "Initial SDL Program" << endl;
    CApp theApp;
    return theApp.OnExecute();
}


// int lua_GetAmnt(lua_State *luaState){
//     lua_pushinteger(luaState,10);
//     return 1;
// }
// cout << "Testing lua" << endl;
// string szLua = "x=GetAmnt() \n  return (x > 7)";

// lua_State *lState = luaL_newstate();
// try{
//     lua_register(lState,"GetAmnt",lua_GetAmnt);

//     luaL_openlibs(lState);
//     if(luaL_loadstring(lState, szLua.c_str())){
//         throw runtime_error("load string failed");
//     }

//     if(lua_pcall(lState,0,LUA_MULTRET,0)){
//         throw runtime_error("pcall failed");
//     }

//     if((int) lua_toboolean(lState,-1)){
//         cout << "success" << endl;
//     }else{
//         cout << "failure" << endl;
//     }

// }catch(runtime_error){
//     cout << "lua error: " << lua_tostring(lState,-1) << endl;
// }

// lua_close(lState);
// cout << "Post Lua Test" << endl;

