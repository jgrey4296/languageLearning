using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class SoundManager : MonoBehaviour {

    public AudioSource fxSource;
    public AudioSource musicSource;
    
    public static SoundManager instance = null;

    public float lowPitchRange = 0.95f;
    public float hightPitchRange = 1.05f;

    
    
	// Use this for initialization
	void Awake () {
        if (instance == null){
            instance = this;
        }else if(instance != this){
            Destroy(this.gameObject);
        }
        DontDestroyOnLoad(this.gameObject);
        
	}

    public void PlaySingle( AudioClip clip){
        this.fxSource.clip = clip;
        this.fxSource.Play();
    }

    public void RandomizeSFX(params AudioClip [] clips){
        int randomIndex = Random.Range(0,clips.Length);
        float randomPitch = Random.Range(this.lowPitchRange,this.hightPitchRange);

        this.fxSource.pitch = randomPitch;
        this.fxSource.clip = clips[randomIndex];
        this.fxSource.Play();
    }    


}
