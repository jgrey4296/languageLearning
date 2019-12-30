import Data.Map

data Rule = Rule { ruleName :: Maybe String
                 , ruleConditions :: [String]
                 , ruleEffects :: [String]
                 , ruleBindings :: Data.Map String String
                 }

-- eg: Bucket, Workbench
data PhysicalItem = PhysItem { piName :: String
                             , piProps :: Data.Map String [String]
                             , piVariations :: Data.Map String [String]
                             , piRules :: [Rule]
                             , piActions :: Data.Map String [Rule]
                             } deriving (Eq, Show)

data SocialTool = SocTool { stName :: String
                          , stPhysConditions :: [String]
                          , stSocProps :: Data.Map String [String]
                          , stRules :: [Rule]
                          , stActions :: Data.Map String [Rule]
                          } deriving (Eq, Show)

data SocialRitual = SocRit { srName :: String
                           , srSocProps :: Data.Map String [String]
                           , srPhysConditions :: [String]
                           , srSocConditions :: [String]
                           , srRules :: [Rule]
                           , srActions :: Data.Map String [Rule]
                           } deriving (Eq, Show)






-- Constraint checking
-- all PhyItem names are unique
-- all variables tested and manipulated in ruleConditions/Effects exist
-- all Rule bindings map vars in conditions -> effects















---- inputs -> rule -> outputs   ( comonad -> monad ?)
-- Listener monad? 
