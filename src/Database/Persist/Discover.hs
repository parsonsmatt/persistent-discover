-- |
--
-- @since 0.1.0.0
module Database.Persist.Discover
    ( findPersistentModelFiles
    ) where

import Database.Persist.Discover.Exe (getFilesRecursive, stripSuffix)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.FileEmbed

-- | Returns a list of all files with the @.persistentmodels@ suffix.
--
-- > allFiles :: [FilePath]
-- > allFiles = $$(findPersistentModelFiles "config/models/")
--
-- @since 0.1.0.0
findPersistentModelFiles
    :: FilePath
    -- ^ The root directory to search from.
    -> Q (TExp [FilePath])
findPersistentModelFiles root = do
    projectRoot <- makeRelativeToProject root
    files <- runIO $ filter isPersistentModelFile <$> getFilesRecursive projectRoot
    liftTyped files
  where
    isPersistentModelFile filename =
        case stripSuffix ".persistentmodels" filename of
            Just _ ->
                True
            _ ->
                False
