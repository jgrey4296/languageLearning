using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;
using UnityEngine.SceneManagement;

public abstract class MovingObject : MonoBehaviour {

    public float moveTime = 0.1f;
    public LayerMask blockingLayer;
    
    private BoxCollider2D boxCollider;
    private Rigidbody2D rb2D;
    private float inverseMoveTime;
    private SpriteRenderer spriteRenderer;
    private bool facingRight = true;
    
    
	// Use this for initialization
	protected virtual void Start () {
		this.boxCollider = this.GetComponent<BoxCollider2D>();
        this.rb2D = this.GetComponent<Rigidbody2D>();
        this.spriteRenderer = this.GetComponent<SpriteRenderer>();
        this.inverseMoveTime = 1f / this.moveTime;
	}

    protected bool Move(int xDir, int yDir, out RaycastHit2D hit){
        Vector2 start = this.transform.position;
        Vector2 end = start + new Vector2(xDir,yDir);
        this.boxCollider.enabled = false;
        hit = Physics2D.Linecast(start,end,blockingLayer);
        this.boxCollider.enabled = true;

        if (hit.transform == null){
            Debug.Log("Starting Movement");
            if (this.facingRight && xDir == -1){
                this.spriteRenderer.flipX = !this.spriteRenderer.flipX;
                this.facingRight = !this.facingRight;
            }else if(!this.facingRight && xDir == 1){
                this.spriteRenderer.flipX = !this.spriteRenderer.flipX;
                this.facingRight = !this.facingRight;
            }
            
            StartCoroutine(SmoothMovement(end));
            return true;
        }else{
            Debug.Log("Hit Something");
        }
        
        return false;        
    }

    protected virtual void AttemptMove<T>(int xDir, int yDir) where T : Component {
        RaycastHit2D hit;
        bool canMove = Move(xDir, yDir, out hit);

        if (hit.transform == null){
            Debug.Log("Did not Hit a Transform");
            return;
        }
        T hitComponent = hit.transform.GetComponent<T>();
        if (!canMove && hitComponent != null){
            this.OnCantMove(hitComponent);
        }
    }

    
    protected IEnumerator SmoothMovement(Vector2 end){
        //Debug.Log("Smooth Move");
        //Note: this.transform.position doesnt update correctly, use rb2D
        float sqrRemainingDistance = (this.rb2D.position - end).sqrMagnitude;

        while(sqrRemainingDistance > float.Epsilon){
            //Debug.Log(String.Format("Remaining Distance: {0}",sqrRemainingDistance));
            Vector3 newPosition = Vector3.MoveTowards(this.rb2D.position, end,
                                                      this.inverseMoveTime * Time.deltaTime);
            this.rb2D.MovePosition(newPosition);
            sqrRemainingDistance = (this.rb2D.position - end).sqrMagnitude;
            yield return null;
        }
    }

    protected abstract void OnCantMove<T>(T Component) where T: Component;
    
}
