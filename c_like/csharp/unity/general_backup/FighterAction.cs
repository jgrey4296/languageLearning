using System;
using System.Collections.Generic;
using BattleSystem;

namespace BattleSystem
{
	public abstract class FighterAction
	{
		//All Actions a fighter can perform are subclassed from this.

		public FighterAction () {
			
		}

		public void Act(Fighter actor, Fighter target, List<Fighter> bystanders){
			//The Actor performs an action on the recipients, possibly on the bystanders
			//Eg: Compare actor.attack to recipients.defense, then reduce health by attack.power,
			//and add status confused. Bystanders may gain state scared.

		}

		public float calculateUtility(Fighter actor, Fighter target, List<Fighter> bystanders){

			return 0.0f;
		}
	}
}

