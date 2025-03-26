
module PackageType (
  PackageData
  )

where
  data PackageData = PackageData
   { 
    id           :: Int
  , weight       :: Int
  , arrivalTime  :: Int
  , deadlineTime :: Int
  , destination  :: Int
   } deriving (Show)
