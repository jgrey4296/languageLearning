using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class NetworkObject : MonoBehaviour {

	protected virtual void Start() {
		NetworkScript.GetInstance().register (this);
	}

	public abstract string toPythonString();

}
