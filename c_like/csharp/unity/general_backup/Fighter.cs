using System;
using System.Linq;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using BattleSystem;
using Unitilities.Tuples;

namespace BattleSystem {

	public class Fighter : MonoBehaviour {

		//Text file containing stats, tokens and states to populate the fighter with
		public  TextAsset statConfig;
		//The action classes that the agent can perform.
		//Todo: expand to include action groups
		public  List<FighterAction> availableActions;
		//The ways in which utility can be assessed
		public  List<FighterUtilityCalc> utilityCalculations;

		//The modifiable float etc Stats of the fighter. Eg: Health, speed etc.
		private Dictionary<FighterStats, float> stats = new Dictionary<FighterStats, float>();
		//The immutable string characteristics of the fighter. Eg: type (fire, water, grass..)
		private Dictionary<FighterToken, string> tokens = new Dictionary<FighterToken, string>();
		//The mutable boolean states of the fighter. Eg: confused, angry, depressed.
		private HashSet<FighterStates> states = new HashSet<FighterStates> ();


		//Private queue of effects to apply / unapply
		//will Have an empty list for each turn up to the further turn currently cared about.
		//each turn will pop off the most recent, and apply all StateUpdates,
		//StateUpdates could Add/Remove a state, or modify a stat. 
		//private Queue<List<StateUpdate>> timedUpdates = new Queue<List<StateUpdate>>();


		// Use this for initialization
		void Start () {
			//init the stats from the text asset
			if (statConfig) {
				//TODO: Load Config data into stats, tokens, and states

				int i = 0;
				foreach (var line in statConfig.text.Split(new string[] { "\n" }, System.StringSplitOptions.None)) {
					Debug.Log (i++.ToString () + " : " + line);
				}

			}

		}

		public bool IsAlive(){
			return GetStat (FighterStats.Health) > 0;
		}

		public bool CanAct(){
			//Return false if there is a state that is blocking action. eg: paralysed.
			return false;
		}

		//Calculate the utility for potential TARGETS
		//'This' holds n strategies of assessing, and runs through them
		public List<Tuple<Fighter, float>> CalculateTargetUtilities(List<Fighter> availableTargets){
			List<Tuple<Fighter, float>> utilities = new List<Tuple<Fighter, float>> ();

			foreach (var target in availableTargets){
				//For Each target, use the available utilityCalculations to determine the utility of the target
				float current_utility = 0.0f;
				foreach (var calc in utilityCalculations) {
					current_utility += calc.calculateUtility (this, target, current_utility);
				}
				utilities.Add (new Tuple<Fighter, float> (target, current_utility));
			}

			return utilities;
		}

		//Calculate the utility for potential ACTIONS
		//unlike calcTargets, calcActions passes off consideration of different strategies to the action itself.
		public List<Tuple<FighterAction, float>> CalculateActionUtilities(List<FighterAction> availableActions, Fighter target, List<Fighter> bystanders){
			List<Tuple<FighterAction, float>> utilities = new List<Tuple<FighterAction, float>> ();
			foreach (var action in availableActions) {
				float utility = action.calculateUtility (this, target, bystanders);
				utilities.Add (new Tuple<FighterAction, float> (action, utility));
			}
			return utilities;
		}


		public void Act(List<Fighter> availableTargets){
			//Select a target / targets
			List<Tuple<Fighter, float>> targetUtilities = CalculateTargetUtilities(availableTargets);
			//Select from the highest utilities
			//TODO: get the utility value of the top third of utilities, use that instead of 0.7
			float minTargetUtilityValue = 0.7f;
			var queryTargets = targetUtilities.Where( t => t.second > minTargetUtilityValue).ToList();
			Fighter target = queryTargets [UnityEngine.Random.Range (0, queryTargets.Count)].first;

			//TODO: Add utility calculation to selection
			List<Tuple<FighterAction, float>> actionUtilities = CalculateActionUtilities(availableActions, target, availableTargets );
			float minActionUtilityValue = 0.7f;
			var queryActions = actionUtilities.Where (t => t.second > minActionUtilityValue).ToList ();
			FighterAction act = queryActions[UnityEngine.Random.Range(0, queryActions.Count)].first;


			//Perform it
			act.Act(this, target, availableTargets);
		}

		public void TickStateUpdates(){
			//if the stateupdate queue is not empty,
			//dequeue the next list of actions
			//apply each one.
		}

		public float GetStat(FighterStats f){
			return stats [f];
		}

		public void ChangeStat(FighterStats f, float delta){
			//modify a stat
			//ie: X gained 0.2 strength, X lost 0.4 speed
			stats[f] += delta;
		}

		public void AddState(FighterStates f){
			//add the state to the states set
			//ie: X is now confused
			states.Add(f);
		}

		public void RemoveState(FighterStates f){
			//Remove the state from the states set
			//ie: X is no longer confused
			states.Remove(f);
		}

		public string GetToken(FighterToken f){
			return tokens [f];
		}
	}

}