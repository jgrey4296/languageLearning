
data Production = Production { inputs :: [String]
                             , transform :: [String]
                             , outputs :: [String]
                             , scope :: String
                             }

type Source = Production { inputs = [] }
type Sink = Production { outputs = [] }


