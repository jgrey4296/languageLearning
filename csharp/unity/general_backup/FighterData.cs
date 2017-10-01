using System;

namespace BattleSystem
{
	public enum FighterStats
	{
		Attack,
		Defense,
		Speed,
		Accuracy,
		Health,
		Sanity,
		Level,
		Experience
	}

	public enum FighterToken
	{
		Type_1,
		Type_2
	}

	[Flags()]
	public enum FighterStates
	{
		Alive,
		Dead,
		Asleep,
		Angry,
		Confused,
		Depressed	
	}

}

