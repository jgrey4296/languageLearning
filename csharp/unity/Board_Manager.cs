using System; //for serializable, allows foldouts
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using Random = UnityEngine.Random;

public class Board_Manager : MonoBehaviour {

    [Serializable] //Allow display in inspector
	public class Count {
        public int minimum;
        public int maximum;

        public Count( int min, int max){
            minimum = min;
            maximum = max;
        }
	}

    //Modifiable parameters of the board generator
    public int columns = 8;
    public int rows = 8;
    public Count wallCount = new Count(5,9);
    public Count foodCount = new Count(1,5);
    public GameObject Exit;
    public GameObject[] floorTiles;
    public GameObject[] wallTiles;
    public GameObject[] foodTiles;
    public GameObject[] enemyTiles;
    public GameObject[] outerwallTiles;
    public GameObject Player;

    //The central point to organise the board around
    private Transform boardHolder;
    //All of the positions in the board, emptied as the algorithm goes
    private List<Vector3> gridPositions = new List<Vector3>();
    
    //generate the board position coordinates, not the instances themselves
    void InitialiseList(){
        Debug.Log("Initialising List");
        this.gridPositions.Clear();
        for( int x = 1; x < this.columns; x++){
            for( int y = 1; y < this.rows; y++){
                this.gridPositions.Add(new Vector3(x,y,0f));
            }
        }
    }

    //setup floor tiles, and the outer wall
    void BoardSetup(){
        Debug.Log("Starting Board Setup");
        GameObject Board = new GameObject("Board");
        //place board in front of the camera:
        Board.transform.position = Camera.main.transform.position;
        Board.transform.Translate(new Vector3(0f,0f,1f));
        this.boardHolder = Board.transform;
        for(int x = -1; x < this.columns + 1; x++){
            for(int y = -1; y < this.rows + 1; y++){
                GameObject toInstantiate;
                if (x == -1 || x == this.columns || y == -1 || y == this.rows){
                    toInstantiate = this.outerwallTiles[Random.Range(0, this.outerwallTiles.Length)];
                }else{
                    toInstantiate = this.floorTiles[Random.Range(0, this.floorTiles.Length)];
                }

                GameObject instance = Instantiate(toInstantiate, new Vector3(x-(this.columns/2),
                                                                             y-(this.rows/2),
                                                                             0f),
                                                  Quaternion.identity) as GameObject;
                instance.transform.SetParent(this.boardHolder,false);
            }
        }
    }

    //select a random position from the list of possible positions
    Vector3 RandomPosition(){
        int randomIndex = Random.Range(0, this.gridPositions.Count);
        Vector3 randomPosition = this.gridPositions[randomIndex] - new Vector3(this.columns/2,
                                                                               this.rows/2,
                                                                               0.0f);
        this.gridPositions.RemoveAt(randomIndex);
        return randomPosition;
    }

    //Given a set of objects, put them down randomly in the open spots on the board
    void LayoutObjectAtRandom(GameObject[] tileArray, int min, int max){
        int objectCount = Random.Range(min,max+1);

        for (int i = 0; i < objectCount; i++){
            Vector3 randomPosition = this.RandomPosition();
            GameObject tileChoice = tileArray[Random.Range(0, tileArray.Length)];
            GameObject instance = Instantiate(tileChoice, randomPosition, Quaternion.identity);
            instance.transform.SetParent(this.boardHolder,false);
            
        }
    }

    //Generate the entire board
    public void SetupScene(int level){
        this.BoardSetup();
        this.InitialiseList();
        Debug.Log(String.Format("Creating Walls: {0}:{1}",wallCount.minimum, wallCount.maximum));
        this.LayoutObjectAtRandom(wallTiles, wallCount.minimum, wallCount.maximum);
        Debug.Log(String.Format("Creating Food: {0}:{1}", foodCount.minimum,foodCount.maximum));
        this.LayoutObjectAtRandom(foodTiles, foodCount.minimum, foodCount.maximum);
        int i = (int) Mathf.Log(level,2f);
        Count enemyCount = new Count(i,i);
        Debug.Log(String.Format("Creating Enemies: {0}:{1}", enemyCount.minimum, enemyCount.maximum));
        this.LayoutObjectAtRandom(enemyTiles, enemyCount.minimum, enemyCount.maximum);
        Debug.Log("Creating Exit");
        GameObject exit = Instantiate(this.Exit, new Vector3(this.columns-1 - (this.columns/2),
                                                             this.rows-1 - (this.rows/2),0f),
                                      Quaternion.identity);
        exit.transform.SetParent(this.boardHolder,false);
        Debug.Log("Creating Player");
        GameObject player = Instantiate(this.Player, new Vector3(1 - (this.columns/2),
                                                                 1 - (this.rows/2), 0f),
                                        Quaternion.identity);
        player.transform.SetParent(this.boardHolder,false);

    }
    

}
