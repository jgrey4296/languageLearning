using System.Collections;
using System.Collections.Generic;
using System.Linq;
using UnityEngine;
using BattleSystem;

namespace BattleSystem {
	public class Battle_Manager : MonoBehaviour {
		//TODO: Make this a singleton?

		//All the fighters in the battle
		public List<Fighter> fighters;
		//All the ACTIVE fighters in the battle
		//TODO: Change this to a priority queue?
		private Queue<Fighter> initiativeOrder;


		void SetupBattle(){
			//Start a new battle by populating the list of active fighters
			initiativeOrder = new Queue<Fighter> (fighters.OrderBy (o => o.GetStat (FighterStats.Speed)));

		}

		void FinishBattle(){
			initiativeOrder.Clear ();
			fighters.Clear ();
		}

		void TakeTurn(){
			//Fail out quickly if theres no one active
			if (IsFinished ()) {
				return;
			}

			//Pop off
			Fighter current = initiativeOrder.Dequeue();
			if (current.IsAlive ()) {
				//current.TickStateUpdates ();

				if (current.CanAct ()) {
					//Act
					List<Fighter> fighterList = new List<Fighter> (initiativeOrder);
					current.Act (fighterList);
				}

				//Add whoever just moved to the end of the queue
				initiativeOrder.Enqueue(current);
				//TODO: detect speed changes and resort as appropriate
			}
			//Do Nothing, and don't re-add self to the queue
		}

		void AddFighter(Fighter f){
			fighters.Add (f);
		}

		void RemoveFighter(Fighter f){
			fighters.Remove (f);
		}

		bool IsFinished(){
			//Todo: later add teams
			if (initiativeOrder.Count > 1) {
				return true;
			}
			return false;
		}

	}

}