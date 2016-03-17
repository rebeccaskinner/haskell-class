module ParseFile where

import Data.List.Split

data Customer = Customer { customerFirstName :: String
                         , customerLastName  :: String
                         , customerDoB :: String
                         , customerBalance :: Float } deriving (Eq,Show)

parseCustomer :: String -> Either String Customer
parseCustomer line = parseLine <$> validateLine line
  where
    validateLine line = validateFieldCount line
    validateFieldCount line =
      let fields = wordsBy (==',') line in
      if 4 == length fields then Right line else Left "Invalid number of fields"
    parseLine csvData =
      let [first,last,dob,balance] = wordsBy (==',') csvData in
      Customer first last dob (read balance)

parseCustomers :: String -> Either String [Customer]
parseCustomers = sequence . (map parseCustomer) . lines
