using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Loader : MonoBehaviour {

    public Game_Manager gameManager;
    
	// Use this for initialization
	void Awake () {
        if(Game_Manager.instance == null){
            Instantiate(this.gameManager);
        }        
	}
}
