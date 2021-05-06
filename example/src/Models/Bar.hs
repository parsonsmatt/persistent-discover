module Models.Bar where

import Database.Persist.TH

mkPersist sqlSettings [persistLowerCase|

Bar
    name    String
    age     Int
    deriving Eq Show

|]
