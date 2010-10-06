module Ak.Db (
    -- * DbSpec Utilities
    DbSpec
  , discoverDbSpec
  , discoverDbSpecFrom

    -- * Path Utilities
  , directoryParents
  ) where

import Control.Monad
    ( filterM )
import System.Directory
    ( getCurrentDirectory, doesFileExist )
import System.FilePath
    ( takeDirectory, (</>) )
import Ak.Common
    ( taskFilename )

-- | The DbSpec describes all parent paths where a .ak file exists.
-- There is no guarantee that this is the full set of .ak files that
-- will be used when re-assembling the database, as any of them could
-- be marked as roots.
type DbSpec = [FilePath]

-- | Generate a list of the parents of a filename.
directoryParents :: FilePath -> [FilePath]
directoryParents path
  | parent == path = []
  | otherwise      = path : directoryParents parent
  where
  parent = takeDirectory path

-- | Discover a @DbSpec@ from any directory
discoverDbSpecFrom :: FilePath -> IO DbSpec
discoverDbSpecFrom  = filterM doesFileExist . map (</> taskFilename) . directoryParents

-- | Discover a @DbSpec@ from the current directory.
discoverDbSpec :: IO DbSpec
discoverDbSpec  = discoverDbSpecFrom =<< getCurrentDirectory
