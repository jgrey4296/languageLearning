using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class SwitchToKinematicOnGrounding : MonoBehaviour {
	/*
	 * Simple script to turn off physics for a dropped house, when it hits the floor,
	 * simplifies placement of houses.
	 */
	private Rigidbody2D m_Rigidbody2D;

	void Awake(){
		m_Rigidbody2D = GetComponent<Rigidbody2D> ();
	}

	void OnCollisionEnter2D (Collision2D col)
	{
		if(col.gameObject.CompareTag("Floor"))
		{
			m_Rigidbody2D.isKinematic = true;
		}
	}
}
