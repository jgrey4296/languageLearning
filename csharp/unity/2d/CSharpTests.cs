using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CSharpTests : MonoBehaviour {

	// Use this for initialization
	void Start () {
        Debug.Log("Testing a lambda");
        Func<int,int,bool> a = (x,y) => x == y;
        Debug.Log(a(5,4));

        Debug.Log("Testing an instantly called lambda");
        new Action(() => Debug.Log("Hello instant"))();

        Debug.Log("Testing a multiline lambda");
        new Action(() => {
                Debug.Log("MultiLine");
                Debug.Log("Lambda");
            })();

        Debug.Log("Testing an Action lambda");
        Func<int, bool> myAction = (b) => {
            b += 1;
            return b == 4;
        };
        Debug.Log(myAction(2));
            
            
        try{
            Debug.Log("Testing an Exception");
            throw new Exception("Blah");
        }catch (Exception e){
            Debug.Log("Caught an exception");
            Debug.Log(e);
        }
	}
	
	// Update is called once per frame
	void Update () {
		
	}
}
