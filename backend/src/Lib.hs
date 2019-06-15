{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Lib
    ( runServer
    ) where


import Control.Monad.Reader
import Control.Monad.IO.Class      (MonadIO(..))
import Control.Monad.Logger        (MonadLogger(..), LoggingT, Loc, LogSource, LogLevel, toLogStr)
import Control.Monad.IO.Unlift     (MonadUnliftIO)
import Data.Time
import Data.Text
import Database.Esqueleto
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH (mkDeleteCascade, mkMigrate, mkPersist, share, persistLowerCase, sqlSettings)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO

data Env = Env
  { foo :: String
  }

newtype App a = App { 
  runApp :: ReaderT Env IO a
} deriving (Monad, Functor, Applicative, MonadIO, MonadUnliftIO)

instance MonadLogger App where
  monadLoggerLog _ _ _ msg = liftIO $ putStrLn $ show $ toLogStr msg

share [ mkPersist sqlSettings
      , mkDeleteCascade sqlSettings
      , mkMigrate "migrateAll"] [persistLowerCase|
  Class
    name Text
    createdAt UTCTime default=CURRENT_TIME
    createdBy TeacherId
    deriving Show
  
  Teacher
    fullName Text
    createdAt UTCTime default=CURRENT_TIME
    email Text
    UniqueEmail email
    deriving Show

  Student
    fullName Text
    classId ClassId
    deriving Show
|]

type Api = "test" :> Get '[JSON] String

testApi :: Proxy Api
testApi = Proxy

runServer :: IO ()
runServer = do
  let port = 4000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve testApi server

server :: Server Api
server = getTest

getTest :: Handler String
getTest = return "foo"

runDb :: ConnectionString -> SqlPersistT App a -> App a
runDb connectionString query = do
  withPostgresqlConn connectionString $ \backend -> runReaderT query backend

setupDb :: SqlPersistT IO ()
setupDb = do
  runMigration migrateAll