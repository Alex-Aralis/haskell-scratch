import qualified Data.Map as Map

type Code = String

data LockerState = Taken | Free deriving (Show, Eq)

type LockerMap = Map.Map Integer (LockerState, Code)
type Failure = String

lookupLocker :: LockerMap -> Integer -> Either Failure Code
lookupLocker lockerMap lockerNumber = 
  case Map.lookup lockerNumber lockerMap of 
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " not found."
    Just (state, code) -> if state == Free
                          then Right code
                          else Left $ "Locker number " ++ show lockerNumber ++ " is taken :(."

lockers = Map.fromList
  [(100,(Taken,"ZD39I"))  
  ,(101,(Free,"JAH3I"))  
  ,(103,(Free,"IQSA9"))  
  ,(105,(Free,"QOTSA"))  
  ,(109,(Taken,"893JJ"))  
  ,(110,(Taken,"99292"))  
  ] 

main = putStrLn . show $ lookupLocker lockers 110 
