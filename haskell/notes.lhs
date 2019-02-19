-- The balance between rational and irrational 
-- Macro-economic reasoning, Micro-economic and behavioural economics
-- A Cif-like reasoning system where assess volitions that can determine
-- the ratios of reasoning types applied to a situation.

-- internally balance groups of rules, and then combine them in a weight way?
-- rationality as the unweighted assessment, then modified more and more by personal evaluations?

-- scientific theories: testable, refutable, falsifiable

-- rete net where rules, if activated, will disconnect other unique alternatives
-- to save unnecessary evaluations. save the disconnections to reconnect on
-- falsification of the leading rule

-- Not can be expressed as weak !a OR strong -a.
-- In which case: a true thing can be !(-a OR !a)
-- !a is weaker than -a

-- --------------------------------------------------
-- CiF
-- --------------------------------------------------
--  eg: (nextTo Bob Bill)
type Fact String String

--  eg: Angry 5, Sad 10
type Volition String Int | None

--  eg: greet, harass. Any of the actions performable
-- Following string array: parameters
type Intention String [String]

--  eg: if (hasGreeted x y) -> (Friends 5)
--  prior evaluated volitions as the second param
--  m being the category of rule
rule m :: [Fact] -> [Volition] -> ([Volition],[Intention])
triggerRule m :: [Fact] -> ([Fact],[Rule])

-- take a set of rules, and facts, and produce volitions and intentions 
evalRules :: [rule] -> [Fact] -> ([Volition], [Intention])
evalTriggers :: [rules] -> [Fact] -> ([Fact],[Rule])

aggregateSortedVolition :: [Volition] -> [Volition]
aggregateSortedIntention :: [Intention] -> [Intention]

selectAppropriateRules :: [Volition] -> [Intention] -> [Rule] -> [Rule]

-- potential intentions + volition amounts -> A Specific intention to perform
decide :: [Intention] -> [Volition] -> Intention

-- perform intention:
perform :: Intention -> [Facts]

CiF rules facts :: [rules] -> [facts] -> CiF rules` facts`
CiF rules facts = do
  (volitions,intentions) <- evalRules rules facts
  sortedVolitions <- aggregateSortedVolition volitions
  sortedIntentions <- aggregateSortedIntention intentions
  selectedIntention <- decide sortedIntentions sortedVolitions
  results <- perform selectedIntention
  (newFacts,newRules) <- evalTriggers rules (facts + results)
  _ <- CiF (rules + newRules) (facts + results + newFacts)
                   
-- --------------------------------------------------
-- Versu
-- --------------------------------------------------

rule :: [Fact] -> Intention
utility :: Intention -> [Rule] -> [Fact] -> Volition
intentions :: [rule] -> [Intention]
utilities :: [Intention] -> [Rule] -> [Fact] -> [Volition]
decide :: [Intention] -> [Volition] -> Intention
perform :: Intention -> ([Fact],[Rule])



-- --------------------------------------------------
-- prototype system
-- --------------------------------------------------

