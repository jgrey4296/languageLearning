using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using BattleSystem;

namespace BattleSystem {
	public abstract class FighterUtilityCalc {

		public float calculateUtility(Fighter self, Fighter target, float current_utility){
			//Compare self to the target,
			//return a value in the range -1.0 - 1.0 of how to modify the utility towards this target

			return 0;
		}				
	}
}
