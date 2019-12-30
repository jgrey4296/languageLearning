Institutions. A Way of conceptualising social interactions and their interrelation
Let institutions exist independently of individuals,

> data Institution = Institution {
>   instName :: [String]
>   , instNameModifiers :: [NameMod]
>   , principle :: InstitutionPrinciple           --The guiding principle
>   , physicalObjects :: [PhysicalObject]         --physical objects of the institution
>   , locations :: [Location String]              --physical locations of the institution
>   , communicationChannels :: [(CommChannel)]    --the channels by which meaning can be communicated in the institution
>   -- Norms:
>   , membershipNorms :: [Norm]                   --who the institution encompasses, dynamically
>   , constitutiveNorms :: [Norm]                 -- what the concepts/actions in the institution are
>   , regulativeNorms :: [Norm]                   -- how to use the concepts/actions
>   -- Broader Field:
>   , relatedInstitutions :: [(Institution, [Norm])]
>   , unrelatedInstitutions :: [Institution]
>   , internalInstitutions :: [(Institution, [Norm])]
>   -- Change
>   , dynamics :: [DynamicRule String]
>   , events :: [Event]
>   }

Institutions are applied to Individuals

> data Individual a b = Individual {
>   indName :: [String]                           --all possible refering names
>   , indNameModifiers :: [NameMod] 
>   , communicationChannelStates :: [CommChannelState a]
>   , location :: Location b
>   , heldObjects :: [PhysicalObject]
>   , stats :: [Stat]
>   }

> data PhysicalObject = PhysicalObject {
>   objName :: [String]
>   , effects :: [Fact]
>   }-- eg: hammer, nails, playing card, house, door, trouses
> data Location a = Location { -- eg: Kitchen, Building, Neighbourhood, City, Country
>   locName :: [String]                           --all possible refering names
>   , nameModifiers :: [Regex]
>   , scale :: Int
>   , contains :: [a]
>   }
> data CommChannel = CommChannel String -- eg: voice, clothing, tone, appearance, 
> data CommChannelState a = CommChannelState {
>   channel :: CommChannel
>   , state :: [(String, Int)]
>   }


> data Event = Event --TODO

Institutions are means of structuring norms
Norms being social rules

> data Norm = Norm {
>   normName :: String
>   , normPredicates :: [Predicate String]
>   , consequents :: [Consequent]
>   }



> data DynamicRule a = DynamicRule {
>   dynamicPredicates :: [Predicate a]
> }
> data Stat = Stat String Int -- An Arbitrary means of Describing character attributes
> type NameMod = Regex
> data Regex = Regex String

> data Predicate a = Predicate {
>   operator :: Op
>   , negated :: Bool
>   , factType :: String
>   , value :: a
>   }

> type Consequent = Fact;
> data  Op = Exists  | LESS | GREATER | NOT

An institution has a defined principle, guiding the interactions. I'm mainly thinking of Graeber's types here

> data InstitutionPrinciple = Communistic 
>                           | Hierarchic
>                           | Democratic
>                           | Graphic
>                           | Individualistic
>                           deriving (Show, Eq)




Relatedly: Accidents

> data Accident = Fall | Knock | Illness


> class Rule a where
>   truthy        :: [Fact] -> a -> Maybe Bool
>   constitutive  :: [Fact] -> a -> Maybe [Fact]
>   dynamic       :: [Fact] -> [a] -> a -> Maybe [Modification]

> data Fact = Fact -- TODO
> data Modification = Modification -- TODO

> class Institutional a where
>   step :: [Fact] -> a -> (a, [Action]) 

> data Action = Action [Fact]


