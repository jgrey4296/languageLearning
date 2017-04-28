using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.SceneManagement;
using UnityEngine.UI;

public class Game_Manager : MonoBehaviour {

    public static Game_Manager instance = null;
    public float levelStartDelay = 2f;
    public int playerFoodPoints = 100;
    public float turnDelay = .1f;
    [HideInInspector] public bool playersTurn = true;

    
    private Board_Manager boardScript;
    private int level = 0; //test level for appearance of enemies
    private List<Enemy> enemies;
    private bool enemiesMoving;
    private Text levelText;
    private GameObject levelImage;
    private bool doingSetup = false;

    void OnLevelFinishedLoading(Scene scene, LoadSceneMode mode){
        Debug.Log("Initialising Scene");
        this.level++;
        this.InitGame();
    }

    void OnEnable(){
        //delegate for sceneLoaded:
        SceneManager.sceneLoaded += this.OnLevelFinishedLoading;
    }

    void OnDisable(){
        SceneManager.sceneLoaded -= this.OnLevelFinishedLoading;
    }
    
    void Awake(){
        //Make the Game and Board singletons
        if(Game_Manager.instance == null){
            Game_Manager.instance = this;
        }else if(Game_Manager.instance != this){
            Destroy(this.gameObject);
        }
        DontDestroyOnLoad(this.gameObject);
        this.enemies = new List<Enemy>();
        
        this.boardScript = this.GetComponent<Board_Manager>();
        //this.InitGame();
    }
    
    public void GameOver(){
        this.enabled = false;
        this.levelText.text = "After " + this.level + " days, you starved.";
        this.levelImage.SetActive(true);
    }
        
    void InitGame(){
        this.doingSetup = true;
        this.levelImage = GameObject.Find("Level Image");
        this.levelText = GameObject.Find("Level Text").GetComponent<Text>();
        this.levelText.text = "Day " + this.level;
        this.levelImage.SetActive(true);
        Invoke("HideLevelImage",this.levelStartDelay);
        
        this.enemies.Clear();        
        this.boardScript.SetupScene(level);
    }

    private void HideLevelImage(){
        this.levelImage.SetActive(false);
        this.doingSetup = false;
    }
    
    IEnumerator MoveEnemies(){
        this.enemiesMoving = true;
        yield return new WaitForSeconds(this.turnDelay);
        if (this.enemies.Count == 0){
            yield return new WaitForSeconds(this.turnDelay);
        }
        foreach(Enemy en in this.enemies){
                if (!en.gameObject.activeInHierarchy){
                    continue;
                }
                en.MoveEnemy();
                yield return new WaitForSeconds(en.moveTime);
            }
        this.playersTurn = true;
        this.enemiesMoving = false;
    }

    public void Update(){
        if(this.playersTurn || this.enemiesMoving || this.doingSetup){
            return;                
        }
        StartCoroutine(MoveEnemies());
    }


    public void AddEnemyToList(Enemy script){
        this.enemies.Add(script);
    }
}
