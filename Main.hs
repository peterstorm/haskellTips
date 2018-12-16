{-# LANGUAGE 
   DeriveGeneric, GADTs, OverloadedStrings, FlexibleContexts, FlexibleInstances, TypeFamilies, TypeApplications, StandaloneDeriving, TypeSynonymInstances, MultiParamTypeClasses, TemplateHaskell #-}

module Main where

import Control.Lens
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple
import Data.Text (Text)
import Control.Monad.Reader
import Data.Foldable

-- | create datatype for a path, and make it usable by Beam
data PathT f = Path
           { _pathPathName :: C f Text
           , _pathPathDir  :: C f Text
           } deriving Generic

makeLenses ''PathT

type Path = PathT Identity
type PathId = PrimaryKey PathT Identity

deriving instance Show Path
deriving instance Eq Path

instance Beamable PathT
instance Table PathT where
  data PrimaryKey PathT f = PathId (C f Text) deriving Generic
  primaryKey = PathId . _pathPathName

instance Beamable (PrimaryKey PathT)


data UserT f = User
             { _userEmail     :: Columnar f Text
             , _userFirstName :: Columnar f Text
             , _userLastName  :: Columnar f Text
             , _userPassword  :: Columnar f Text
             } deriving Generic

makeLenses ''UserT

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User

instance Beamable UserT
instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Text) deriving Generic
  primaryKey = UserId . _userEmail

instance Beamable (PrimaryKey UserT)

data ShoppingCartDb f = ShoppingCartDb
                      { _shoppingCartUsers :: f (TableEntity UserT)
                      , _pathPathList         :: f (TableEntity PathT)
                      }
                      deriving Generic

makeLenses ''ShoppingCartDb

instance Database be ShoppingCartDb

shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings

data Config = Config 
            { dbConn :: Connection }

data DbConfig = DbConfig
              { _dbConf :: Connection }
makeClassy ''DbConfig

getConnection :: MonadIO m => m Config
getConnection = liftIO $ fmap Config $ open "shoppingcart1.db"

getConnection' :: MonadIO m => m DbConfig
getConnection' = liftIO $ fmap DbConfig $ open "shoppingcart1.db"

type Production = ReaderT Config IO

class (Monad m) => DbOperations m where
  selectAllUsersDebug :: m ()
  selectAllUsers :: m [User]

instance DbOperations Production where
  selectAllUsersDebug = do
    conf <- ask
    liftIO . runBeamSqliteDebug putStrLn (dbConn conf) $ do
    users <- runSelectReturningList $ select (all_ (shoppingCartDb ^. shoppingCartUsers)) 
    traverse_ (liftIO . putStrLn . show) users

  selectAllUsers = do
    conf <- ask
    liftIO . runBeamSqlite (dbConn conf) $ do
    users <- runSelectReturningList $ select (all_ (shoppingCartDb ^. shoppingCartUsers)) 
    pure users

selectAllPathsDebug :: (MonadReader r m, MonadIO m, HasDbConfig r) => m ()
selectAllPathsDebug = do
  conf <- view dbConf
  liftIO . runBeamSqliteDebug putStrLn conf $ do
    paths <- runSelectReturningList $ select (all_ (shoppingCartDb ^. pathPathList))
    traverse_ (liftIO . putStrLn . show) paths

selectAllPaths :: (MonadReader r m, MonadIO m, HasDbConfig r) => m [Path]
selectAllPaths = do
  conf <- view dbConf
  liftIO . runBeamSqlite conf $ do
    paths <- runSelectReturningList $ select (all_ (shoppingCartDb ^. pathPathList))
    pure paths

selectAllUsersDebug' :: (MonadReader r m, MonadIO m, HasDbConfig r) => m ()
selectAllUsersDebug' = do
  conf <- view dbConf
  liftIO . runBeamSqliteDebug putStrLn conf $ do
  users <- runSelectReturningList $ select (all_ (shoppingCartDb ^. shoppingCartUsers)) 
  traverse_ (liftIO . putStrLn . show) users

insertTestPaths :: (MonadReader r m, MonadIO m, HasDbConfig r) => m ()
insertTestPaths = do
  conf <- view dbConf
  liftIO . runBeamSqliteDebug putStrLn conf $ runInsert $
    insert (shoppingCartDb ^. pathPathList) $
      insertValues [ Path "peter" "~/peter"
                   , Path "johanna" "~/"]

insertPath :: (MonadReader r m, MonadIO m, HasDbConfig r) => Text -> Text -> m ()
insertPath name dir = do
  conf <- view dbConf
  liftIO . runBeamSqlite conf $ runInsert $
    insert (shoppingCartDb ^. pathPathList) $
      insertValues [ Path name dir ]



insertValuesDebug :: Connection -> IO ()
insertValuesDebug conn = runBeamSqliteDebug putStrLn conn $ runInsert $
  insert (_shoppingCartUsers shoppingCartDb) $
  insertValues [ User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c"
               , User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f"
               , User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c" ]

sortUsersByFirstName = orderBy_ 
 (\u -> (asc_ (_userFirstName u), desc_ (_userLastName u))) (all_ (shoppingCartDb ^. shoppingCartUsers))

userCount = aggregate_ (\u -> as_ @Int countAll_) (all_ (_shoppingCartUsers shoppingCartDb))

--queryDbDebug :: DbOperations m => m ()
--queryDbDebug = do
  --conf <- ask
  --liftIO . runBeamSqliteDebug putStrLn (dbConn conf) $ do
  --users <- runSelectReturningList $ select (all_ (shoppingCartDb ^. shoppingCartUsers)) 
  --traverse_ (liftIO . putStrLn . show) users

querySortDebug :: Connection -> IO ()
querySortDebug conn = runBeamSqliteDebug putStrLn conn $ do
  users <- runSelectReturningList $ select sortUsersByFirstName
  mapM_ (liftIO . putStrLn . show) users

queryCountUsers :: Connection -> IO ()
queryCountUsers conn = runBeamSqliteDebug putStrLn conn $ do
  count <- runSelectReturningOne $ select userCount
  case count of
    Just c -> liftIO $ putStrLn ("We have " <> show c <> " users in the database")
    Nothing -> error "nothing"

insertPathIO :: Text -> Text -> IO ()
insertPathIO name dir = runReaderT (insertPath name dir) =<< getConnection'

insertPathsIO :: IO ()
insertPathsIO = runReaderT insertTestPaths =<< getConnection'

selectAllPathsIO :: IO [Path]
selectAllPathsIO = runReaderT selectAllPaths =<< getConnection'

selectAllUsersDebugIO :: IO ()
selectAllUsersDebugIO = runReaderT selectAllUsersDebug =<< getConnection

selectAllUsersDebugIO' :: IO ()
selectAllUsersDebugIO' = runReaderT selectAllUsersDebug' =<< getConnection'

selectAllUsersIO :: IO [User]
selectAllUsersIO = runReaderT selectAllUsers =<< getConnection

getEmails :: [User] -> [Text]
getEmails = fmap (^. userEmail)

getPathDir :: [Path] -> [Text]
getPathDir = fmap (^. pathPathDir)

main :: IO ()
main = do
  conn <- open "shoppingcart1.db"
  selectAllUsersDebugIO
  users <- selectAllUsersIO
  print $ getEmails users
  querySortDebug conn
  insertPathIO "hello" "world"
  queryCountUsers conn
  selectAllUsersDebugIO'
