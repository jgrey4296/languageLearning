using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.AI;

public class navSimple : MonoBehaviour {
	public float closeness;
	public Transform[] goal;
	private NavMeshAgent agent;
	// Use this for initialization
	void Start () {
		agent = GetComponent<NavMeshAgent> ();
		agent.destination = goal[Random.Range(0,goal.Length)].position;
	}
	
	// Update is called once per frame
	void Update () {
		if (agent.remainingDistance < closeness){
			Debug.Log ("Switching to new Destination");
			agent.SetDestination (goal [Random.Range (0, goal.Length)].position);
		}


	}


}
