-- |
--
-- @since 0.1.0.0
module Database.Persist.Discover
    ( findPersistentModelFiles
    ) where

import Database.Persist.Discover.Exe (getFilesRecursive, stripSuffix)

-- | Returns a list of all files with the @.persistentmodels@ suffix.
--
-- @since 0.1.0.0
findPersistentModelFiles
    :: FilePath
    -- ^ The root directory to search from.
    -> IO [FilePath]
findPersistentModelFiles root = do
    filter isPersistentModelFile <$> getFilesRecursive root
  where
    isPersistentModelFile filename =
        case stripSuffix ".persistentmodels" filename of
            Just _ ->
                True
            _ ->
                False
