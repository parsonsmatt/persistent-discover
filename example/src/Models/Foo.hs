module Models.Foo where

import Database.Persist.TH

mkPersist sqlSettings [persistLowerCase|

Foo
    name    String
    age     Int
    deriving Eq Show

|]
