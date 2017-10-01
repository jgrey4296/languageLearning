using System.Collections;
using System.Collections.Generic;
using UnityEngine;


public class MapManager : MonoBehaviour {
	public static MapManager instance;

	//SINGLETON
	//Manages the map overall. Provides pathfinding, general instantiation,
	//and locations for everything

	public int mapSize = 24;
	public float tileSize = 1f;
	//A 2D list of locations, by row then column
	List<List<Tile>> tiles = new List<List<Tile>>();
	public List<Actor> actors = new List<Actor> ();
	List<GameObject> targets = new List<GameObject>();
	public GameObject defaultTile;
	public GameObject defaultItem;
	public GameObject defaultActor;
	public GameObject defaultTarget;
	public GameObject defaultClick;

	static int[][] NeighbourMatrix = new int[][] { new int[] {-1, 0}, //Below
		new int[] { 1, 0}, //Above
		new int[] {-1, -1}, //Below Left
		new int[] {1, -1}, //Above Left
		new int[] {0, 1},  //Right
		new int[] {0, -1}, //Left
		new int[] {-1, 1}, //Below Right
		new int[] {1, 1} }; //Above Right


	// Use this for initialization
	void Awake () {
		if (MapManager.instance == null) {
			MapManager.instance = this;
		} else if (MapManager.instance != this) {
			Destroy (this.gameObject);
			return;
		}
		//Create the map base
		this.CreateMap();
	}

	// Update is called once per frame
	void Update () {
		
	}

	void CreateMap(){
		Debug.Log ("Creating Map");
		for (int i = 0; i < mapSize; i++) { //row / y
			var column = new List<Tile>();
			
			for (int j = 0; j < mapSize; j++) { //column / x
				var currentPosition = new Vector3( j * tileSize, i * tileSize, 0f);
				var toInstantiate = Instantiate (defaultTile, currentPosition, Quaternion.identity);
				toInstantiate.transform.SetParent (this.transform, false);
				column.Add (toInstantiate.GetComponent<Tile>());
				//Randomly add an item or an actor up to a specific number
				if (Random.Range (0, 100) < 50) {
					if (Random.Range (0, 100) < 0) {
						//Make an item
						var newItem = Instantiate(defaultItem);
						newItem.transform.SetParent (toInstantiate.transform, false);
						toInstantiate.GetComponent<Tile> ().items.Add (newItem.GetComponent<Item>());
					} else if (Random.Range(0, 100) < 15 && actors.Count < 10) {
						//make an actor
						var newActor = Instantiate(defaultActor);
						newActor.transform.Translate (currentPosition);
						newActor.transform.SetParent (this.transform, false);
						actors.Add (newActor.GetComponent<Actor> ());

						//Create a new target:
						var newTarget = Instantiate(defaultTarget);
						newTarget.transform.Translate (currentPosition);
						newTarget.transform.SetParent (this.transform, false);
						targets.Add (newTarget);

					}
				}

			}
			tiles.Add(column);
		}
			
		this.transform.Translate (new Vector3 (-mapSize * 0.5f, -mapSize * 0.5f, 0f));
	}

	public List<Tile> GetNeighbours(Tile tile){
		Vector3 location = tile.transform.localPosition;
		List<Tile> neighbours = new List<Tile> ();
		foreach (var locMod in NeighbourMatrix){
			Vector3 potentialNeighbour = new Vector3 (Mathf.RoundToInt (location.x + locMod [1]), Mathf.RoundToInt (location.y + locMod [0]), 0f);
			if (potentialNeighbour.x < 0 || potentialNeighbour.y < 0){
				continue;
			}
			if (potentialNeighbour.x >= mapSize || potentialNeighbour.y >= mapSize){
				continue;
			}
			try {
				neighbours.Add (GetTile (potentialNeighbour));
			} catch {
				Debug.Log (string.Format ("Failed to get neighbour: {0}", potentialNeighbour));
			}
		}
		return neighbours;
	}


	//A* Pathfinding:
	//Given two tiles, figure out the best path between them
	public List<Vector3> PathFind( Tile start, Tile end){
		//Debug.Log (string.Format ("Pathfinding from {0} to {1}", start.ToString(), end.ToString()));
		if (end.GetCost () == Mathf.Infinity || start == end) {
			//Early exit if you can't actually go to the specified tile
			return new List<Vector3> ();
		}

		//Intermediate data structures
		HashSet<Tile> discovered = new HashSet<Tile>();
		//Frontier uses a custom comparer to allow duplicate keys
		SortedList<float, Tile> frontier = new SortedList<float, Tile> (new PriorityComparer());
		Dictionary<Tile, Tile> origins = new Dictionary<Tile, Tile> ();
		Dictionary<Tile, float> trueCosts = new Dictionary<Tile, float> ();

		frontier.Add (getDistance(start, end), start);
		trueCosts [start] = 1;

		while (frontier.Count > 0) {
			Tile current = frontier.Values [0];
			frontier.RemoveAt (0);
			if (current == end) {
				return ReconstructPath (start, end, origins);
			}
			discovered.Add (current);
			List<Tile> neighbours = GetNeighbours (current);
			foreach (var neighbour in neighbours) {
				Debug.AssertFormat ((current.transform.localPosition - neighbour.transform.localPosition).sqrMagnitude < 4, 
					"Distance between the current and the neighbour should be minimal: {0} - {1}",
					current.transform.localPosition, neighbour.transform.localPosition);

				if (discovered.Contains(neighbour)){
					continue;
				}
				float cost = neighbour.GetCost();
				if (!frontier.ContainsValue (neighbour)) {
					frontier.Add (cost + getDistance (neighbour, end), neighbour);
				}

				if (!trueCosts.ContainsKey (current)) {
					continue;
				}
				float trueCost = cost + trueCosts[current];
				//Try Get Value used incase the neighbour isn't in the truecosts dictionary yet,
				//and so default to infinity
				float retrievedCost = 0;
				bool result = trueCosts.TryGetValue(neighbour, out retrievedCost);
				if (!result) {
					retrievedCost = Mathf.Infinity;
				}
				//Skip if the cost is worse, but only if there is a stored value 
				if (trueCost >= retrievedCost){
					continue;
				}
				origins[neighbour] = current;
				trueCosts [neighbour] = trueCost;
			}
		}
		//No path found
		return new List<Vector3> ();
	}

	//This is the heuristic used in A*. could be replaced with something else
	public float getDistance(Tile a, Tile b){
		return Vector3.Distance (a.transform.localPosition, b.transform.localPosition);
	}

	//Convert the dictionary of lowest cost moves into an actual path list
	//Path goes from [Target -> Source], but an agent uses it LIFO
	public List<Vector3> ReconstructPath (Tile start, Tile final, Dictionary<Tile, Tile> origins){
		//Debug.Log (string.Format ("Size of origins: {0}", origins.Count));
		List<Vector3> path = new List<Vector3> ();
		Tile current = final;
		while (current != null) {
			Debug.Assert (current.GetCost () < Mathf.Infinity);
			path.Add (current.transform.localPosition);
			Tile next;
			bool result = origins.TryGetValue (current, out next);
			if (!result) {
				next = null;
			} else {
				Debug.Assert ((current.transform.position - next.transform.position).sqrMagnitude < 5);
			}
			current = next;
		}
		if (path [path.Count - 1] != start.transform.localPosition) {
			return new List<Vector3> ();
		}

		if (path.Count > 0) {
			Debug.Log (string.Format ("Start: {0}, First: {0}", start.transform.localPosition, path [path.Count - 1]));
			Debug.AssertFormat ((start.transform.localPosition - (path [path.Count - 1])).sqrMagnitude < 5, 
				"Distance between the start and the first step should be minimal: {0} - {1}",
				start.transform.localPosition, path [path.Count - 1]);
		}

		return path;		
	}

	//Given a location, get the relevant tile
	//Unguarded, will throw errors if given a position out of bounds
	public Tile GetTile(Vector3 v){
		List<Tile> row = tiles [Mathf.RoundToInt (v.y)];
		return row [Mathf.RoundToInt(v.x)];
	}

	
	//The custom comparer for the priority queue.
	//Treats two values that are the same as different.
	private class PriorityComparer : IComparer<float> {
		public int Compare (float x, float y){
			if (x == y) {
				return 1;
			}
			return y.CompareTo (x);
		}
		
	}

	//Sets the location of a debug target
	public void SetDestination(int i, Vector3 v){
		var target = targets [i];
		target.transform.localPosition = v;
	}
		
}
