import Data.Map

type Fact = String
data FactBase = FactBase [Fact]

type Binding = Map String String

data Measurement = Scale Float | Element [String]
data CulturalConcept = CulturalConcept String Measurement

data Regulation = Regulation { regCondition :: FactBase -> Bool 
                             , regModifier :: Action -> Maybe Action }

data Norm = Norm { normAction :: Action
                 , normCondition :: [Fact] -> Binding
                 , normModifier :: Action -> Maybe Action
                 , normSanction :: Sanction }

data Sanction = Sanction { sanctionAction :: Action
                         , sanctionEffects :: Action }

data State = State String

data Action = Action { actionTranstions :: [State -> State]
                     , actionParams :: [String]
                     , actionOutput :: String
                     , actionEffects :: FactBase -> FactBase }



data Activity = Activity { activityStates :: [Action] }

data Institution = Institution { iInputs :: [CulturalConcept]
                               , iOutputs :: [CulturalConcept]
                               , iInternalConcepts :: [CulturalConcept]
                               , iRegulations :: [Regulation]
                               , iNorms :: [Norm]
                               , iSanctions :: [Sanction]
                               , iActivities :: [Activity]
                               , iIGU :: [Institution] }
                               

