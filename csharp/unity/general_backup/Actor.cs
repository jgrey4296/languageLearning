using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Actor : MonoBehaviour {
	//Has Either its own AI, or communicates with python
	private static int MasterIndex = 0;
	//Has an inventory, and other stats, and a location

	public List<Vector3> currentPath = new List<Vector3>();
	private bool moving = false;
	public float speed = 0.05f;
	public float stopBoundary = 0.1f;
	public int index;
	public bool hasPathFound = false;
	public float pauseAmnt = 0.1f;
	public bool notActive = true;
	// Use this for initialization
	void Start () {
		index = Actor.MasterIndex;
		Actor.MasterIndex += 1;
	}

	// Update is called once per frame
	void Update () {
		if (notActive) {
			return;
		}


		if (!moving && currentPath.Count == 0) {
			int[] dest = new int[] {
				Random.Range (0, MapManager.instance.mapSize - 1),
				Random.Range (0, MapManager.instance.mapSize - 1)
			};

			Tile current = MapManager.instance.GetTile (transform.localPosition);
			Tile destination = MapManager.instance.GetTile (new Vector3(Random.Range(0, MapManager.instance.mapSize), 
				Random.Range(0, MapManager.instance.mapSize), 0f));
			currentPath = MapManager.instance.PathFind (current, destination);
			foreach (Vector3 loc in currentPath) {
				var newTarget = Instantiate (MapManager.instance.defaultTarget, MapManager.instance.transform, false);
				newTarget.transform.localPosition = loc;
			}

			//MapManager.instance.SetFinalDestination (destination.transform.localPosition);
			//Debug.Log (string.Format ("New Path Count: {0}", currentPath.Count));
		} else if (!moving && currentPath.Count > 0) {
			//move to the next position
			var next = currentPath[currentPath.Count - 1];
			currentPath.RemoveAt (currentPath.Count - 1);
			StartCoroutine(pathfind(next));
			if (currentPath.Count == 0) {
				hasPathFound = true;
			}
		}
		
	}

	IEnumerator pathfind(Vector3 target){
		//todo: Move a dot to the target position
		MapManager.instance.SetDestination(index, target);

		moving = true;
		float remainingDistance = (transform.localPosition - target).sqrMagnitude;
		if (remainingDistance > 5) {
			Debug.Log (string.Format ("Distance: {0}", remainingDistance));
			Debug.Log (string.Format ("Start: {0}", transform.localPosition));
			Debug.Log (string.Format ("Target: {0}", target));
			Debug.Break ();
		}
		//Get the difference vector,
		while (remainingDistance > stopBoundary) {
			//move towards the vector
			Vector3 newPosition = Vector3.MoveTowards(transform.localPosition, target, speed * Time.deltaTime);
			transform.localPosition = newPosition;
			//recalculate distance
			remainingDistance = (transform.localPosition - target).sqrMagnitude;
			yield return null;
		}
		yield return new WaitForSeconds (pauseAmnt);
		moving = false;
	}

	IEnumerator Wait (float seconds){
		yield return new WaitForSeconds (seconds);
	}


}
