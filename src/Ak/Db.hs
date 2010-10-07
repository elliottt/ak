module Ak.Db (
    -- * Task Database Specifications
    DbSpec
  , discoverDbSpec
  , discoverDbSpecFrom

    -- * Generic Task Storage
  , Tasks(..)

    -- * Task Databases
  , Db()
  , loadDb
  , writeDb

    -- * Path Utilities
  , directoryParents

    -- * Task Stores
  , TaskStore()
  , emptyTaskStore
  , touch
  , makeRoot
  , loadTaskStore
  , writeTaskStore
  ) where

import Ak.Types
import Ak.IO

import Control.Exception
    ( bracket )
import Control.Monad
    ( filterM )
import System.Directory
    ( getCurrentDirectory, doesFileExist )
import System.FilePath
    ( takeDirectory, (</>) )
import System.IO
    ( Handle, openFile, hPutStrLn, hClose, hPutStr, hPrint
    , IOMode(ReadWriteMode) )
import Ak.Common
    ( taskFilename )

-- Database Specifications -----------------------------------------------------

-- | The DbSpec describes all parent paths where a .ak file exists.  There is
-- no guarantee that this is the full set of .ak files that will be used when
-- re-assembling the database, as any of them could be marked as roots.
newtype DbSpec = DbSpec [FilePath]

-- | Generate a list of the parents of a filename.
directoryParents :: FilePath -> [FilePath]
directoryParents path
  | parent == path = []
  | otherwise      = path : directoryParents parent
  where
  parent = takeDirectory path

-- | Discover a @DbSpec@ from any directory
discoverDbSpecFrom :: FilePath -> IO DbSpec
discoverDbSpecFrom  = fmap DbSpec
                    . filterM doesFileExist
                    . map (</> taskFilename)
                    . directoryParents

-- | Discover a @DbSpec@ from the current directory.
discoverDbSpec :: IO DbSpec
discoverDbSpec  = discoverDbSpecFrom =<< getCurrentDirectory


-- Generic Task Stores ---------------------------------------------------------

class Tasks store where
  -- | Add a @Task@ to the store.
  addTask :: Task -> store -> store


-- Task Stores -----------------------------------------------------------------

data TaskStore = TaskStore
  { tsDirty :: Bool
  , tsRoot  :: Bool
  , tsPath  :: FilePath
  , tsTasks :: [Task]
  } deriving (Show)

instance Tasks TaskStore where
  addTask t ts = touch ts { tsTasks = t:tsTasks ts }

emptyTaskStore :: FilePath -> TaskStore
emptyTaskStore path = TaskStore
  { tsDirty = True
  , tsRoot  = False
  , tsPath  = path
  , tsTasks = []
  }

-- | Promote a @TaskStore@ to a root.
makeRoot :: TaskStore -> TaskStore
makeRoot ts = ts { tsRoot = True }

-- | Touching the @TaskStore@ turns it dirty, signaling that it needs to be
-- re-written to disk.
touch :: TaskStore -> TaskStore
touch ts = ts { tsDirty = True }

-- | Load a TaskStore from the disk, raising an exception if it either doesn't
-- exist, or could not be parsed.
loadTaskStore :: FilePath -> IO TaskStore
loadTaskStore path = do
  text         <- readFile path
  (root,tasks) <- error "need to parse tasks" text
  return TaskStore
    { tsDirty = False
    , tsRoot  = root
    , tsPath  = path
    , tsTasks = tasks
    }

-- | Write a @TaskStore@ to disk, only if it is dirty.
writeTaskStore :: TaskStore -> IO ()
writeTaskStore ts
  | not (tsDirty ts) = return ()
  | otherwise        = withTaskFile ts $ \ h -> do
    writePrologue h ts
    mapM_ (writeTask h) (tsTasks ts)

-- | Write the .ak file header
writePrologue :: Handle -> TaskStore -> IO ()
writePrologue h ts = hPutStrLn h
                   $ unwords
                   [ show (tsDirty ts)
                   , show (tsRoot ts)
                   ]

-- | Open the .ak file for reading/writing, and run the continuation with its
-- handle.
withTaskFile :: TaskStore -> (Handle -> IO ()) -> IO ()
withTaskFile ts =
  bracket (openFile (tsPath ts) ReadWriteMode) hClose


-- Task Databases --------------------------------------------------------------

-- | Task databases are just a collection of task stores, representing the .ak
-- files as they occur when traversing up the filesystem.
newtype Db = Db [TaskStore]

instance Tasks Db where
  addTask _ (Db [])       = Db []
  addTask t (Db (ts:tss)) = Db (addTask t ts:tss)

-- | Given a @DbSpec@, load a @Db@.
loadDb :: DbSpec -> IO Db
loadDb (DbSpec paths) = error "loadDb"

-- | Write out a @Db@ to the filesystem.
writeDb :: Db -> IO ()
writeDb (Db tss) = mapM_ writeTaskStore tss
