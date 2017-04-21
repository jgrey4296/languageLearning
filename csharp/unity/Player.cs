using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.SceneManagement;

public class Player : MovingObject {

    public int wallDamage = 1;
    public int pointsPerFood = 10;
    public int pointsPerSoda = 20;
    public float restartLevelDelay = 1f;

    public AudioClip moveSound1;
    public AudioClip moveSound2;
    public AudioClip eatSound1;
    public AudioClip eatSound2;
    public AudioClip drinkSound1;
    public AudioClip drinkSound2;
    public AudioClip gameOverSound;
    
    
    private Text foodText;    
    private Animator animator;
    private int food;
    
    
	// Use this for initialization
	protected override void Start () {
        this.animator = this.GetComponent<Animator>();
        this.food = Game_Manager.instance.playerFoodPoints;
        this.foodText = GameObject.Find("Food Text").GetComponent<Text>();
        this.foodText.text = "Food: " +  this.food;
        base.Start();
	}

    private void OnDisable(){
        Game_Manager.instance.playerFoodPoints = this.food;
    }
    
	// Update is called once per frame
	void Update () {
		if(!Game_Manager.instance.playersTurn) {
            return;
        }
        Debug.Log("Getting player input");
        
        int horizontal = 0;
        int vertical = 0;

        horizontal = (int) Input.GetAxisRaw("Horizontal");
        vertical = (int) Input.GetAxisRaw("Vertical");

        if(horizontal != 0){
            vertical = 0;
        }
        if (horizontal != 0 || vertical != 0){
            Debug.Log(String.Format("Attempting to move: ({0},{1})",horizontal,vertical));
            this.AttemptMove<Wall>(horizontal, vertical);
        }
	}

    protected override void AttemptMove<T>(int xDir, int yDir){
        this.food--;
        this.foodText.text = "Food: " + this.food;
        base.AttemptMove<T>(xDir,yDir);
        RaycastHit2D hit;
        if(this.Move(xDir,yDir, out hit)){
            SoundManager.instance.RandomizeSFX(this.moveSound1,this.moveSound2);
        }
        this.CheckIfGameOver();
        
        Game_Manager.instance.playersTurn = false;
    }

    private void CheckIfGameOver(){
        if (this.food <= 0){
            Debug.Log("Player is Dead");
            SoundManager.instance.PlaySingle(this.gameOverSound);
            SoundManager.instance.musicSource.Stop();
            Game_Manager.instance.GameOver();
        }
    }

    private void OnTriggerEnter2D(Collider2D other){
        Debug.Log("Collision on Enter2d");
        if(other.tag == "Exit"){
            Debug.Log("Collieded with Exit");
            Invoke("Restart", this.restartLevelDelay);
            this.enabled = false;
        }else if(other.tag == "Food"){
            Debug.Log("Collided with Food");
            this.food += this.pointsPerFood;
            this.foodText.text = "+" + this.pointsPerFood + " Food.";
            other.gameObject.SetActive(false);
            SoundManager.instance.RandomizeSFX(this.eatSound1,this.eatSound2);
        }else if(other.tag == "Soda"){
            Debug.Log("Collided with Soda");
            this.food += this.pointsPerSoda;
            this.foodText.text = "+" + this.pointsPerSoda + " Food.";
            other.gameObject.SetActive(false);
            SoundManager.instance.RandomizeSFX(this.drinkSound1,this.drinkSound2);
        }else{
            Debug.Log("Hit Something Else");
        }
        
    }
    
    protected override void OnCantMove<T>(T component){
        Debug.Log("Player Hit a Wall");
        Wall hitWall = component as Wall;
        hitWall.DamageWall(this.wallDamage);
        this.animator.SetTrigger("playerChop");
    }

    private void Restart(){
        Debug.Log("Restarting upon hitting exit");
        SceneManager.LoadScene("MainScene");
    }

    public void LoseFood(int loss){
        this.animator.SetTrigger("playerHit");
        this.food -= loss;
        this.foodText.text = "-" + loss + " Food.";
        this.CheckIfGameOver();
    }
    
}
