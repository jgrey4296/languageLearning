-- Moise Spec as Haskell
type Cardinality = (Int, Int) -- Min Max
data LinkType = Acquaintance | Communication | Authority
data LinkScope = Intra | Inter
data GoalType = Achievement | Maintenance
data PlanType = Sequence | Parallel | Choice
data NormType = Obliged | Permitted
data Inheritance = Inheritance Role Role

-- TODOS
data Agent = Agent String --todo: add info about agent
data Mission = Mission [Condition] -- conditions? mission as a test? 
data Deadline = AbsoluteDeadline Int | RelativeDeadline Int
data Condition = Condition [String] -- exclusion logic? bindable facts

-- Structural Spec
data Role = Role String

data RoleLink = RoleLink { lSource :: String
                         , lTarget :: String
                         , lType :: LinkType
                         , lScope :: LinkScope }

data Group = Group { gRoleCompatibilities :: [(Role, [Role])]
                   , gRoleCardinality :: [(Role, Cardinality)]
                   , gSubGroupCardinality :: [(Group, Cardinality)] }

data InsStructure = InsStructure { sRoles :: [Role]
                                 , sInRel :: [Inheritance]
                                 , sRoot   :: Group
                                 , sGroups :: [Group]
                                 , sLinks :: [RoleLink] }

-- Functional Spec
data InsFunction = InsFunction { fMissions :: [Mission]
                               , fGoals :: [Goal]
                               , fSchemes :: [Scheme] }

data Scheme = Scheme { schemaCardinality :: [(Mission, Cardinality)]
                     , schemaRootGoal :: Goal }


data Goal = Goal { goalMissions :: [Mission]
                 , goalType :: GoalType
                 , goalAgentCardinality :: Cardinality
                 , goalTimeToFulfill :: Deadline
                 , goalPlan :: Plan }

data Plan = Plan { planSubgoals :: [Goal]
                 , planType :: PlanType }

-- Normative Spec
data InsNorms = InsNorm { normActivationConditions :: [Condition]
                        , normRole :: [Role]
                        , normType :: NormType
                        , normMission :: Mission
                        , normDeadline :: Deadline }

-- Organisation Entity Spec
data OrgEntity = OrgEntity { orgStructure :: InsStructure
                           , agents :: [Agent]
                           , groupInstances :: [GroupInstance] }


data GroupInstance = GroupInstance { groupInstanceSpec :: Group
                                   , groupInstancePlayers :: [Agent]
                                   , groupInstanceSubgroups :: [Group]
                                   , groupInstanceResponsibilities :: [Scheme] }

data SchemeInstance = SchemeInstance { schemeInstanceSpec :: Scheme
                                     , schemeInstanceCommitments :: [Commitment]
                                     , schemeInstanceAchievements :: [SchemeAchievement] }
                      
data Commitment = Commitment [(Mission, [Agent])]
data SchemeAchievement = SchemeAchievement [(Agent, [Goal])]




-- Social Norm

type Expectation = (Int, Int)

data SocialNorm = SocialNorm { snCondition :: [Condition]
                             , snBehaviourDesc :: [String]
                             , snEmpiricalExpectation :: Expectation
                             , snNormExpectation :: Expectation
                             , snSanctionExpectation :: Expectation }
