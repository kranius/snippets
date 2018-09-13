{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Aeson

data Worker = Worker
       { workerName :: String
       , workerPosition :: String
       , workerFirstYear :: Int
       }

instance ToJSON Worker where
  toJSON Worker{..} = object [ "name" .= workerName
                             , "position" .= workerPosition
                             , "first-year" .= workerFirstYear
                             ]

instance FromJSON Worker where
  parseJSON = withObject "Worker" $ \o -> do
    workerName <- o .: "name"
    workerPosition <- o .: "position"
    workerFirstYear <- o .: "first-year"
    return Worker{..}

