import Data.Map
-- Basic Aliases
type Name = String
type Tick = Int

data RepaymentStructure = Maturation | Amortized | Mixed deriving (Show, Eq)
data OptionHolderRight = Call | Put deriving (Show, Eq)

-- The main types:
-- A Straight Forward Debt between two individuals
data Debt = Debt { creditor :: Name,
                   debtor :: Name,
                   principal :: Float,
                   debtPremium :: Float,
                   interestRate :: Float,
                   term :: (Tick, Tick),
                   debtRemaining :: Float,
                   debtRepaymentStructure :: RepaymentStructure
                 } deriving (Show, Eq)

-- A Tradable Option on a Future.
data Option = Option { seller :: Name,
                       buyer :: Name,
                       optionAmount :: Float,
                       optionPremium :: Float,
                       strike :: Float,
                       optionType :: OptionHolderRight,
                       maturity :: Tick,
                       optionRemaining :: Float,
                       optionRepaymentStructure :: RepaymentStructure
                     } deriving (Show, Eq)

data Actor = Actor { name :: Name,
                     cashOnHand :: Float,
                     debts :: [Debt],
                     credits :: [Debt],
                     options :: [Option]
                   } deriving (Show, Eq)

-- Holds the credit system state at a particular time
data SystemState = SystemState { actors :: Map Name Actor,
                                 allDebts :: [Debt],
                                 allOptions :: [Option],
                                 time :: Tick
                               } deriving (Show, Eq)
-- Simple Constructor for a Credit System
initState = SystemState { actors = fromList [], allDebts = [], allOptions = [], time = 0 }

-- Basic Modifications to the system state
addActorByName :: Name -> SystemState -> SystemState
addActorByName name state = state { actors = insert name actor oldActors }
  where actor = Actor name 0 [] [] []
        oldActors = actors state

addActor :: Actor -> SystemState -> SystemState
addActor actor state = state { actors = insert (name actor) actor oldActors }
  where oldActors = actors state

removeActor :: Name -> SystemState -> SystemState
removeActor name state = state { actors = delete name oldActors }
  where oldActors = actors state


addCashOnHand :: Name -> Float -> SystemState -> SystemState
addCashOnHand name amnt state = state { actors = adjust modFunc name oldActors }
  where oldActors = actors state
        modFunc = \x -> x { cashOnHand = (cashOnHand x) + amnt }

        
subCashOnHand :: Name -> Float -> SystemState -> SystemState
subCashOnHand name amnt state = state { actors = adjust modFunc name oldActors }
  where oldActors = actors state
        modFunc = \x -> x { cashOnHand = (cashOnHand x) - amnt }

-- Constructors for Debts and Options 
createDebt   :: (Name, Name) -> [Float] -> (Tick, Tick) -> RepaymentStructure -> Debt
createDebt namePair values term repayment =
  Debt creditor debtor principal premium rate term currentTotal repayment
  where (creditor, debtor) = namePair
        [principal, premium, rate] = values
        currentTotal = principal + premium
        
  
createOption :: (Name, Name) -> [Float] -> OptionHolderRight -> Tick -> RepaymentStructure -> Option
createOption namePair values holderRight expiration repayment =
  Option seller buyer amount premium strike holderRight expiration currentTotal repayment
  where (seller, buyer) = namePair
        [amount, premium, strike] = values
        currentTotal = amount + premium


-- Add the debt to the actor, updating cash on hand as necessary 
addDebtToActor :: Debt -> Actor -> Actor
addDebtToActor debt actor = actor { cashOnHand = updatedCash, debts = updatedDebts, credits = updatedCredits }
  where isDebtor = name actor == debtor debt
        updatedCash = if isDebtor then currentCash + principal' else currentCash - principal'
          where currentCash = cashOnHand actor
                principal' = principal debt
        updatedDebts = if isDebtor then debt : debts actor else debts actor
        updatedCredits = if not isDebtor then debt : credits actor else credits actor

removeDebtFromActor :: Debt -> Actor -> Actor
removeDebtFromActor debt actor = actor { debts = remaining }
  where remaining = Prelude.filter (\x -> x /= debt) $ debts actor

-- Add the debt to the state, updating actors as necessary
-- only modifies if the creditor and debtor exists
addDebtToState   :: Debt -> SystemState ->  SystemState
addDebtToState debt state = state { actors = updatedActors }
  where startingActors = actors state
        updates = do { upCreditor <- Data.Map.lookup (creditor debt) startingActors;
                       upDebtor <- Data.Map.lookup (debtor debt) startingActors;
                       if name upCreditor == name upDebtor then Nothing
                       else let newCreditor = addDebtToActor debt upCreditor
                                newDebtor = addDebtToActor debt upDebtor
                                moddedMap = insert (creditor debt) newCreditor $ insert (debtor debt) newDebtor startingActors
                            in return $ moddedMap
                     }
        updatedActors = guardedActorsUpdate startingActors updates

-- either return the unmodified actor map, or the new one, if its not nothing.
guardedActorsUpdate :: Map Name Actor -> Maybe (Map Name Actor) -> Map Name Actor
guardedActorsUpdate originals updated = case updated of
  Just updates -> updates
  Nothing -> originals
  


-- addOption :: Option -> SystemState -> SystemState
isDebtMature :: Debt -> Tick -> Bool
isDebtMature debt tick = tick == (snd $ term debt)

isDebtExpired :: Debt -> Tick -> Bool
isDebtExpired debt tick = tick > (snd $ term debt)

isOptionMature :: Option -> Tick -> Bool
isOptionMature option tick = tick == maturity option

isOptionExpired :: Option -> Tick -> Bool
isOptionExpired option tick = tick > maturity option

isLiquid :: Actor -> Bool
isLiquid actor = cashOnHand actor  > 0

--shouldEnactOption :: Option -> Bool

getDebts :: Name -> SystemState -> Maybe [Debt]
getDebts name state = do { actor <- Data.Map.lookup name $ actors state; return $ debts actor; }

getCredits :: Name -> SystemState -> Maybe [Debt]
getCredits name state = do { actor <- Data.Map.lookup name $ actors state; return $ credits actor; }

getOptions :: Name -> SystemState -> Maybe [Option]
getOptions name state = do { actor <- Data.Map.lookup name $ actors state; return $ options actor; }



-- tickState  :: SystemState -> SystemState

-- tickDebt   :: Debt -> Actor -> Actor -> (Debt, Actor, Actor)
-- All debts: tick up the debtRemaining
-- Amortized debts: tick down the debtRemaining, transfer from debtor -> creditor


-- tickOption :: Option -> Actor -> Actor -> (Option, Actor, Actor)
-- All Options: assess whether to enact the option, tick up the option remaining amount
-- Amortized options: tick down the option remaining, transfer from buyer -> seller?



---------- Utilities
simpleActor1 = Actor "Bob" 100 [] [] []
simpleActor2 = Actor "Bill" 100 [] [] []
simpleActors = [simpleActor1, simpleActor2]
simpleDebt = Debt "Bob" "Bill" 50 10 0.1 (0, 10) 60 Amortized

badDebt = Debt "Jill" "Bob" 50 10 0.1 (0, 10) 60 Amortized

simpleState = withDebt
  where start = initState
        withActors = Prelude.foldl (\s a -> addActor a s) start simpleActors
        withDebt = addDebtToState simpleDebt withActors

