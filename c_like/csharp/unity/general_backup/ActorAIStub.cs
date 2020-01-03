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
public class ActorAIStub : MonoBehaviour
{
	private PlatformerCharacter2D m_Character;
	private int waitAmnt = 2;
	private bool undecided = true;
	private float direction = 0.0f;

	private void Awake ()
	{
		m_Character = GetComponent<PlatformerCharacter2D> ();
	}

	public void Start(){
		SpeechBubbleManager.Instance.AddSpeechBubble (transform, "Blaaahhh");
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
		m_Character.Move (direction, false, false);
	}
		
	IEnumerator chooseDirection (){
		undecided = false;
		direction = (float)-0.2f
		+ (0.4f
		* ((float)UnityEngine.Random.Range (0, 100)
		* 0.01f));
		yield return new WaitForSeconds (this.waitAmnt);
		undecided = true;
	}
}
