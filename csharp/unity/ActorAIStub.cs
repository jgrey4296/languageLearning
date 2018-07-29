using System;
using UnityEngine;
using UnityStandardAssets.CrossPlatformInput;
using UnityStandardAssets._2D;
using System.Collections;
using VikingCrewTools.UI;


/**
 * A Stub for an agent to select actions
 */
[RequireComponent (typeof(PlatformerCharacter2D))]
public class ActorAIStub : NetworkObject
{
	private string agentId;
	private PlatformerCharacter2D m_Character;
	public int waitAmnt = 2;
	private bool undecided = true;
	private float direction = 0.0f;
	public float speed = 0.2f;
	public float tolerance = 0.5f;
	private RoomDetector room_detector;
	private SpeechBubbleBehaviour m_Speech;

	private Vector2 move_target;

	public void commandAction(ActionCommand ac){
		Debug.Log("Not implemented yet: commandAction");
		this.commandDescription = ac;
		this.busy = true;
	}
	//To pass an action from python AI to a character to perform,
	//which keeps track then updates AI upon completion
	private bool busy;
	private ActionCommand commandDescription;


	private void Awake ()
	{
		this.m_Character = GetComponent<PlatformerCharacter2D> ();
		this.room_detector = GetComponent<RoomDetector> ();
		this.move_target = this.transform.position;
	}

	public void Start(){
		base.Start ();
		this.Speak ("Blaaah");
	}


	private void Update ()
	{
		if (undecided) {
			StartCoroutine (chooseDirection());
		}
	}
		
	private void FixedUpdate ()
	{
		// Pass all parameters to the character control script.
		m_Character.Move (this.direction * this.speed, false, false);
	}


	IEnumerator chooseDirection (){
		this.undecided = false;		

		this.FaceDirection (true);
		yield return new WaitForSeconds (2.0f);
		this.FaceDirection (false);
		yield return new WaitForSeconds (2.0f);
		this.FaceDirection (true);
		yield return new WaitForSeconds (2.0f);

		this.TargetToPointInRoom (0.0f, this.tolerance);
		yield return new WaitForSeconds (this.waitAmnt);
		this.TargetToPointInRoom (1.0f, this.tolerance);
		yield return new WaitForSeconds (this.waitAmnt);
		this.TargetToPointInRoom (0.5f, this.tolerance);
		yield return new WaitForSeconds (this.waitAmnt);

		this.undecided = true;
	}

	IEnumerator checkTarget(){
		float distance = Vector2.Distance (this.transform.position, this.move_target);
		while (distance > this.tolerance) {
			this.Speak ("Moving");
			this.direction = this.transform.position.x < this.move_target.x ? 1.0f : -1.0f;
			distance = Vector2.Distance (this.transform.position, this.move_target);
			yield return null;
		}
		this.Speak ("Got there");
		this.direction = 0.0f;
	}

	private void FaceDirection(bool right){
		this.m_Character.FaceDirection (right);
	}

	private void MoveToRoom(int id){
		//check floor

		//get floor bounds

		//Store path
	}

	private void TargetToPointInRoom(float position, float tolerance){
		//position : 0.0 - 1.0 : Left - Right room bounds
		//tolerance : the leeway to achieving that position
		var current = this.transform.position;
		var bounds = this.room_detector.currentRoom.GetComponent<RoomController> ().GetBounds ();
		var len = bounds [1].x - bounds [0].x;
		this.move_target = new Vector2 (bounds [0].x + (len * position), current.y);
		StartCoroutine (checkTarget ());

	}

	private void CloseDistanceToTarget(){
		

	}

	private void PerformAnimation(){

	}

	private void ChangeAppearance(){

	}

	private void PlaceItem(){

	}

	private void Speak(String text, int amnt = 20){
		if (this.m_Speech != null && this.m_Speech.Iteration == 0) {
			this.m_Speech.UpdateText (text, amnt);
		} else {
			this.m_Speech = SpeechBubbleManager.Instance.AddSpeechBubble (this.transform, text, timeToLive: amnt);
		}
	}

	private void Crouch(){

	}

	private void Die(){

	}

	public override string toPythonString(){
		return "TODO";
	}

}
